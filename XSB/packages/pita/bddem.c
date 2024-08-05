/*

EMBLEM and SLIPCASE

Copyright (c) 2013, Fabrizio Riguzzi and Elena Bellodi

This package uses the library cudd, see http://vlsi.colorado.edu/~fabio/CUDD/
for the relative license.

*/
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "cudd.h"
#include "cinterf.h"
#include <unistd.h>
#include <sys/types.h>

#ifdef _WIN32
#include <Windows.h>
#endif

#define BUFSIZE 200000
#define LOGZERO log(0.01)
#define CACHE_SLOTS 1
#define UNIQUE_SLOTS 1
#define RETURN_IF_FAIL if (ret!=TRUE) return ret;


typedef struct
{
  int nVal,nRule;
  int firstBoolVar;
  int abducible;
  int query;
} variable;


typedef struct
{
  DdNode *key;
  double value;
} rowel;

typedef struct
{
  int cnt;
  rowel *row;
} tablerow;


typedef struct
{
  DdManager * mgr; //Cudd manager
  int * bVar2mVar; //array that maps Boolean vars to multi-valued vars
  variable * vars; // multivalued variables
  int nVars;  // number of multivalued variables
  double * probs; // probabilities of Boolean variables
  int  boolVars;  // number of Boolean variables
  int nRules;  // number of rules
  int * rules; // array with the number of head atoms for each rule
  int n_abd;
  int n_abd_boolVars;
} environment;

typedef struct
{
  environment * env; // one environment for each example
  int ex;  // number of examples
  double * sigma; // sigma array for taking into account deleted paths
  double ***eta;  // eta array: for each rule, each Bool var stores two doubles
  double ***eta_temp; // eta array storing the contribution of the current example
  int * rules; // array with the number of head atoms for each rule
  int * tunable_rules; // array with 1 if the parameters of the rule are tunable, 0 otherwise
  int nRules; // number of rules
  double **arrayprob; //value of paramters. One value ofr each rule and Bool var
  double * nodes_probs;
  tablerow * nodesB; // tables of probabilities for nodes in Backward step
  tablerow * nodesFE; // tables of probabilities for nodes in Forward step
  tablerow * nodesFO; // tables of probabilities for nodes in Forward step
  double * example_prob; // probability (frequency) of examples in the data
  double alpha; // type of parameter initialization in EM: 
                // 0 for truncated Dirichlet process
                // >0 for symmetric Dirichlet distribution with values alpha
} example_data;

typedef struct
{
  int var,val;
} assign;

typedef struct explan
{
  assign a;
  struct explan * next;
} explan_t;

typedef struct
{
  double prob;
  explan_t * mpa;
} prob_abd_expl;

typedef struct
{
  DdNode *node;
  int comp;
} explkey;

typedef struct
{
  explkey key;
  prob_abd_expl value;
} explrowel;

typedef struct
{
  int cnt;
  explrowel *row;
} expltablerow;




double ret_prob_pl(long env_int, long node_int);
double ret_abd_prob_pl(long env_int, long node_int, prolog_term *expl);
double ret_map_prob_pl(long env_int, long node_int, prolog_term *expl);
double ret_vit_prob_pl(long env_int, long node_int, prolog_term *expl);


double Prob(DdNode *node,environment *env,tablerow *table);
prob_abd_expl abd_Prob(DdNode *node,environment *env,expltablerow *expltable,
  tablerow *table,
  int comp_par);
prob_abd_expl map_Prob(DdNode *node, environment * env,
    expltablerow * maptable, tablerow * table,
    int comp_par);
prob_abd_expl vit_Prob(DdNode *node, environment * env,
  expltablerow * expltable, tablerow * table,
  int comp_par);

void end_ex_pl(long ex_d_int);

long init_ex_pl(long ex_d_int);
long add_var_pl(long env_int, prolog_term probTerm, int nRule);
int add_query_var_pl(long env_int, prolog_term probTerm, int nRule);
int add_abd_var_pl(long env_int, prolog_term probTerm, int nRule);
long equality_pl(long env_int, int varIndex, int value);
long and_pl(long env_int, long node1_int, long node2_int);
long one_pl(long env_int);
long zero_pl(long env_int);
long or_pl(long env_int, long node1_int, long node2_int);
long bdd_not_pl(long env_int, long node_int);
void create_dot_pl(long env_int, long node_int, char * filename);
void create_dot_string_pl(long env_int, long node_int, char ** string);
long init_pl();
void end_pl(long env_int);


long init_em_pl();
void end_em_pl(long ex_d_int);
double em_pl(example_data * ex_d_int, prolog_term ruleInfo, prolog_term nodesTerm, double ea,
  double er, int iter, prolog_term *out);

void reorder_pl(long env_int);
long make_query_var_pl(long env_int, int varIndex);

void init_par(example_data * ex_d, prolog_term ruleHeadsTerm);

double ProbPath(example_data * ex_d,DdNode *node, int nex);
//static int rec_deref(void);
void Forward(example_data * ex_d,DdNode *node, int nex);
void UpdateForward(example_data * ex_d,DdNode * node, int nex,
  DdNode *** nodesToVisit,int * NnodesToVisit);
double GetOutsideExpe(example_data *ex_d,DdNode *root,double ex_prob, int nex);
void Maximization(example_data * ex_d);
static double Expectation(example_data *ex_d,DdNode **nodes_ex, int lenNodes);
int reorder_int(environment *env);

FILE *open_file(char *filename, const char *mode);
tablerow* init_table(int varcnt);
double * get_value(tablerow *tab,  DdNode *node);
void add_or_replace_node(tablerow *tab, DdNode *node, double value);
void add_node(tablerow *tab, DdNode *node, double value);
void destroy_table(tablerow *tab,int varcnt);
expltablerow* expl_init_table(int varcnt);
prob_abd_expl * expl_get_value(expltablerow *tab,  DdNode *node, int comp);
void expl_add_node(expltablerow *tab, DdNode *node, int comp, prob_abd_expl value);
void expl_destroy_table(expltablerow *tab,int varcnt);

int length(prolog_term list);


void write_dot(environment * env, DdNode * bdd, FILE * file);

explan_t * insert_ass(assign assignment,explan_t * head);
explan_t * duplicate(explan_t * head);
void free_list(explan_t * head);

void clist_to_pllist(explan_t *mpa, environment * env, prolog_term out);
void abd_clist_to_pllist(explan_t *mpa, prolog_term out);
void vit_clist_to_pllist(explan_t *mpa, environment * env, prolog_term out);


void rand_seed(int seed);
double uniform_sample_pl();
double gauss_sample_pl(double mean,double var);
double gamma_sample_pl(double shape, double scale);
double gamma_sample_gt1(double shape);
void dirichlet_sample_c(double * alpha,int k, double * theta);
void symmetric_dirichlet_sample_c(double alpha,int k, double * theta);

void dirichlet_sample_pl(prolog_term alphaterm,prolog_term* sample_term);
void symmetric_dirichlet_sample_pl(double alpha,int k,prolog_term* sample_term);
int discrete_sample_pl(prolog_term thetaterm);
void initial_values_pl(long ex_d_int, double alpha);



double uniform_sample_pl()
{
  return ((double)rand())/RAND_MAX;
}

double gauss_sample_pl(double mean,double var)
{
  double u1,u2,r,theta,s; 

  u1= uniform_sample_pl();
  u2= uniform_sample_pl();
  r= sqrt(-2*log(u1));
  theta=2*M_PI*u2;
  s=r*cos(theta);
  return sqrt(var)*s+mean;
}

double gamma_sample_pl(double shape, double scale)
{
  double u,s;
  if (shape>=1)
    return gamma_sample_gt1(shape)*scale;
  else
  {
    u=uniform_sample_pl();
    s=gamma_sample_gt1(shape+1);
    return pow(s*u,1/shape)*scale;
  }
}
double gamma_sample_gt1(double shape)
{
  double c,d,x,v,u;

  d=shape-1.0/3.0;
  c =1.0/sqrt(9.0*d);

  do
  {
    do
    {
      x=gauss_sample_pl(0.0,1.0);
      v=pow(1+c*x,3);
    } while (v<=0);
    u=uniform_sample_pl();
  } while (u>=1-0.0331*pow(x,4) && log(u)>=0.5*pow(x,2)+d*(1-v+log(v)));
  return d*v;
}


void symmetric_dirichlet_sample_pl(double alpha,int k,prolog_term* sample_term)
{
  double * sample;

  int i;
  prolog_term out, head, new_out, tail;

  sample=malloc(sizeof(double)*k);

  symmetric_dirichlet_sample_c(alpha,k,sample);
  *sample_term=p2p_new();
  out=p2p_new();
  c2p_nil(out);
  for (i=0;i<k;i++)
  {
    new_out=p2p_new();
    c2p_list(new_out);
    head=p2p_car(new_out);
    c2p_float(sample[i],head);
    tail=p2p_cdr(new_out);
    p2p_unify(tail,out);
    out=new_out;
  }
  p2p_unify(*sample_term,out);

}

void dirichlet_sample_pl(prolog_term alphaterm,prolog_term* sample_term)
{
  double * alpha, * sample;

  int i, k;
  prolog_term out, head, new_out, tail;


  k=length(alphaterm);
  
  alpha=malloc(sizeof(double)*k);
  sample=malloc(sizeof(double)*k);

  for (i=0;i<k;i++)
  {
    head=p2p_car(alphaterm);
    alphaterm=p2p_cdr(alphaterm);
    alpha[i]=p2c_float(head);
  }
  dirichlet_sample_c(alpha,k,sample);
  out=p2p_new();
  c2p_nil(out);
  for (i=0;i<k;i++)
  {
    new_out=p2p_new();
    c2p_list(new_out);
    head=p2p_car(new_out);
    c2p_float(sample[i],head);
    tail=p2p_cdr(new_out);
    p2p_unify(tail,out);
    out=new_out;
  }
  *sample_term=p2p_new();
  p2p_unify(*sample_term,out);
}

int discrete_sample_pl(prolog_term thetaterm)
{
  double * theta;
  double u, p;

  int i, k;
  prolog_term head;

  k=length(thetaterm);
  theta=malloc(sizeof(double)*k);

  for (i=0;i<k;i++)
  {
    head=p2p_car(thetaterm);
    theta[i]=p2c_float(head);
    thetaterm=p2p_cdr(thetaterm);
  }
  u=uniform_sample_pl();
  i=0;
  p=theta[0];
  while (u>p && i<k)
  {
    i++;
    p=p+theta[i];
  }
  free(theta);
  return i;
}

void symmetric_dirichlet_sample_c(double alpha,int k, double * theta)
{
  int i;
  double * alphas;

  alphas=malloc(sizeof(double)*k);

  for (i=0;i<k;i++)
    alphas[i]=alpha;
  dirichlet_sample_c(alphas,k,theta);
  free(alphas);
}

void dirichlet_sample_c(double * alpha,int k, double * theta)
{
  int i;
  double sum;
  double * gamma;

  gamma=malloc(sizeof(double)*k);

  sum=0.0;
  for (i=0;i<k;i++)
  {
    gamma[i]=gamma_sample_pl(alpha[i],1.0);
    sum=sum+gamma[i];
  }
  for (i=0;i<k;i++)
    theta[i]=gamma[i]/sum;
  free(gamma);
}

long init_em_pl()
{
  example_data * ex_d;

  ex_d=(example_data *)malloc(sizeof(example_data));

  ex_d->ex=0;
  ex_d->nRules=0;
  ex_d->env=NULL;
  ex_d->eta=NULL;
  ex_d->eta_temp=NULL;
  ex_d->rules=NULL;
  ex_d->nodes_probs=NULL;
  ex_d->tunable_rules=NULL;
  ex_d->arrayprob=NULL;
  ex_d->alpha=0.0;

  return((long)ex_d);

}

void initial_values_pl(long ex_d_int, double alpha)
{
  example_data * ex_d;

  ex_d=(example_data *)ex_d_int;
  ex_d->alpha=alpha;
}
long init_ex_pl(long ex_d_int)
{
  example_data * ex_d;
  DdManager * mgr;
  int ex;

  
  ex_d=(example_data *) ex_d_int;
  ex=ex_d->ex;
  ex_d->env=(environment *) realloc(ex_d->env, (ex+1)*sizeof(environment));
  ex_d->env[ex].mgr=Cudd_Init(0,0,UNIQUE_SLOTS,CACHE_SLOTS,5120);
  mgr=ex_d->env[ex].mgr;
  Cudd_AutodynEnable(mgr, CUDD_REORDER_GROUP_SIFT);
  Cudd_SetMaxCacheHard(mgr, 0);
  Cudd_SetLooseUpTo(mgr, 0);
  Cudd_SetMinHit(mgr, 15);

  ex_d->env[ex].bVar2mVar=NULL;

  ex_d->env[ex].vars=NULL;

  ex_d->env[ex].nVars=0;

  ex_d->env[ex].probs=NULL;

  ex_d->env[ex].boolVars=0;

  ex_d->env[ex].nRules=ex_d->nRules;

  ex_d->env[ex].rules=ex_d->rules;


  return((long) (ex_d->env+ex));

}

void end_ex_pl(long ex_d_int)
{
  example_data *ex_d;
  
  ex_d=(example_data *)ex_d_int;
  ex_d->ex=ex_d->ex+1;
}

long init_pl()
{
  environment * env;

  env=(environment *)malloc(sizeof(environment));
  // env->mgr=Cudd_Init(0,0,UNIQUE_SLOTS,CACHE_SLOTS,0);
  env->mgr=Cudd_Init(0,0,CUDD_UNIQUE_SLOTS,CACHE_SLOTS,0);
  env->n_abd=0;
  env->n_abd_boolVars=0;

  //Cudd_AutodynEnable(env->mgr, CUDD_REORDER_GROUP_SIFT);
  Cudd_SetMaxCacheHard(env->mgr, 0);
  Cudd_SetLooseUpTo(env->mgr, 0);
  Cudd_SetMinHit(env->mgr, 15);

  env->bVar2mVar=NULL;
  env->vars=NULL;
  env->nVars=0;
  env->probs=NULL;
  env->boolVars=0;
  env->nRules=0;
  env->rules= NULL;
  env->n_abd=0;
  env->n_abd_boolVars=0;
  
  return((long) env);
}

void end_pl(long env_int)
{
  environment *env;

  env=(environment *)env_int;

  Cudd_Quit(env->mgr);

  free(env->bVar2mVar);
  free(env->vars);
  free(env->probs);
  free(env->rules);
  free(env);

}



static double Expectation(example_data * ex_d,DdNode **nodes_ex,int lenNodes)
{
  int i;
  double rootProb,CLL=0;

  for(i=0;i<lenNodes;i++)
  {
    if (!Cudd_IsConstant(nodes_ex[i]))
    {
      ex_d->nodesB=init_table(ex_d->env[i].boolVars);
      ex_d->nodesFE=init_table(ex_d->env[i].boolVars);
      ex_d->nodesFO=init_table(ex_d->env[i].boolVars);

      Forward(ex_d,nodes_ex[i],i);
      rootProb=GetOutsideExpe(ex_d,nodes_ex[i],ex_d->example_prob[i],i);

      if (rootProb<=0.0)
        CLL = CLL + LOGZERO*ex_d->example_prob[i];
      else
        CLL = CLL + log(rootProb)*ex_d->example_prob[i];

      ex_d->nodes_probs[i]=rootProb;
      destroy_table(ex_d->nodesB,ex_d->env[i].boolVars);
      destroy_table(ex_d->nodesFE,ex_d->env[i].boolVars);
      destroy_table(ex_d->nodesFO,ex_d->env[i].boolVars);
    }
    else
      if (nodes_ex[i]==Cudd_ReadLogicZero(ex_d->env[i].mgr))
      {
        CLL=CLL+LOGZERO*ex_d->example_prob[i];
	ex_d->nodes_probs[i]=0.0;
      }
      else
        ex_d->nodes_probs[i]=1.0;
  }
  return CLL;
}

void end_em_pl(long ex_d_int)
{
  int r,i;
  example_data * ex_d;
  ex_d=(example_data *)ex_d_int;

  for (i=0;i<ex_d->ex;i++)
  {
    Cudd_Quit(ex_d->env[i].mgr);
    free(ex_d->env[i].bVar2mVar);
    free(ex_d->env[i].vars);
    free(ex_d->env[i].probs);
  }

  free(ex_d->env);
  for (r=0;r<ex_d->nRules;r++)
  {
    if (ex_d->tunable_rules[r])
    {
      for (i=0;i<ex_d->rules[r]-1;i++)
      {
        free(ex_d->eta[r][i]);
        free(ex_d->eta_temp[r][i]);
      }
      free(ex_d->eta[r]);
      free(ex_d->eta_temp[r]);
    }
  }
  free(ex_d->eta);
  free(ex_d->eta_temp);
  free(ex_d->rules);
  free(ex_d);
}


double ret_prob_pl(long env_int, long node_int)
{
  environment * env;
  DdNode * node;
  tablerow * table;
  double prob;

  env=(environment *)env_int;
  node=(DdNode*) node_int;

  if (!Cudd_IsConstant(node))
  {
    table=init_table(env->boolVars);
    prob=Prob(node,env,table);
    if (Cudd_IsComplement(node))
      prob=1.0-prob;
    destroy_table(table,env->boolVars);
  }
  else
  {
    if (node==Cudd_ReadOne(env->mgr))
      prob=1.0;
    else
      prob=0.0;
  }

  return(prob);
}

int reorder_int(environment *env)
{
  int i,j,var_ind,abd_ind=0,ind=env->n_abd_boolVars;
  variable var,* vars=env->vars;
  DdManager *mgr=env->mgr;
  int boolVars=env->boolVars;
  int * permutation;
  int * bVar2mVar=env->bVar2mVar;

  permutation=malloc(boolVars*sizeof(int));
  for (i=0;i<boolVars;i++)
  {
    j=Cudd_ReadInvPerm(mgr,i);
    var_ind=bVar2mVar[j];
    var=vars[var_ind];
    if (var.abducible || var.query)
    {
      permutation[abd_ind]=j;
      abd_ind++;
    }
    else
    {
      permutation[ind]=j;
      ind++;
    }

  }
  return Cudd_ShuffleHeap(mgr,permutation);
}

void reorder_pl(long env_int)
{
  environment * env;

  env=(environment *)env_int;
  reorder_int(env);
}

double ret_abd_prob_pl(long env_int, long node_int, prolog_term *expl)
{
  environment * env;
  DdNode * node;
  expltablerow * expltable;
  tablerow * table;
  //abdtablerow * abdtable;
  prob_abd_expl delta;
  double p;
  explan_t * mpa;

  env=(environment *)env_int;
  node=(DdNode*) node_int;

  reorder_int(env);
  *expl=p2p_new();

  if (!Cudd_IsConstant(node))
  {
    expltable=expl_init_table(env->boolVars);
    table=init_table(env->boolVars);
    //abdtable=init_abd_table(env->n_abd);
    delta=abd_Prob(node,env,expltable,table,0);
    p=delta.prob;
    mpa=delta.mpa;
    //destroy_table(abdtable,env->n_abd);
    abd_clist_to_pllist(mpa,*expl);
    expl_destroy_table(expltable,env->boolVars);
    destroy_table(table,env->boolVars);
  }
  else
  {
    if (node==Cudd_ReadOne(env->mgr))
      p=1.0;
    else
      p=0.0;
    c2p_nil(*expl);
  }
  return p;
}

double ret_map_prob_pl(long env_int, long node_int, prolog_term *expl)
{
  environment * env;
  DdNode * node;
  expltablerow * maptable;
  tablerow * table;
  //abdtablerow * abdtable;
  prob_abd_expl delta;
  double p;
  explan_t * mpa;

  env=(environment *)env_int;
  node=(DdNode*) node_int;

  reorder_int(env);
  *expl=p2p_new();

  if (!Cudd_IsConstant(node))
  {
    maptable=expl_init_table(env->boolVars);
    table=init_table(env->boolVars);
    //abdtable=init_abd_table(env->n_abd);
    delta=map_Prob(node,env,maptable,table,0);
    p=delta.prob;
    mpa=delta.mpa;
    //destroy_table(abdtable,env->n_abd);
    clist_to_pllist(mpa,env,*expl);
    expl_destroy_table(maptable,env->boolVars);
    destroy_table(table,env->boolVars);
  }
  else
  {
    if (node==Cudd_ReadOne(env->mgr))
      p=1.0;
    else
      p=0.0;
    c2p_nil(*expl);
  }
  return p;
}

double ret_vit_prob_pl(long env_int, long node_int, prolog_term *expl)
{
  environment * env;
  DdNode * node;
  expltablerow * expltable;
  tablerow * table;
  //abdtablerow * abdtable;
  prob_abd_expl delta;
    //abdtable=init_abd_table(env->n_abd);
  double p;
  explan_t * mpa;

  env=(environment *)env_int;
  node=(DdNode*) node_int;

  *expl=p2p_new();

  if (!Cudd_IsConstant(node))
  {
    expltable=expl_init_table(env->boolVars);
    table=init_table(env->boolVars);
    //abdtable=init_abd_table(env->n_abd);
    delta=vit_Prob(node,env,expltable,table,0);
    p=delta.prob;
    mpa=delta.mpa;
    //destroy_table(abdtable,env->n_abd);
    vit_clist_to_pllist(mpa,env,*expl);
    expl_destroy_table(expltable,env->boolVars);
    destroy_table(table,env->boolVars);
  }
  else
  {
    if (node==Cudd_ReadOne(env->mgr))
      p=1.0;
    else
      p=0.0;
    c2p_nil(*expl);
  }

  return p;
}

long make_query_var_pl(long env_int, int varIndex)
{
  environment * env;
  int i,j;
  DdNode * cons, * tmp0,* tmp1, * tmp2, * vari, * varlast, * varj, * or, * tmpor;
  variable var;

  env=(environment *)env_int;

  var=env->vars[varIndex];

  cons=Cudd_ReadOne(env->mgr);
  or=Cudd_ReadLogicZero(env->mgr);

  for (i=var.firstBoolVar; i<var.firstBoolVar+var.nVal-1; i++)
  {    
    vari=Cudd_bddIthVar(env->mgr,i);
    tmpor=Cudd_bddOr(env->mgr,or,vari);
    Cudd_Ref(tmpor);
    Cudd_RecursiveDeref(env->mgr,or);
    or=tmpor;
    for(j=i+1; j<var.firstBoolVar+var.nVal; j++)
    {      
      varj=Cudd_bddIthVar(env->mgr,j);
      tmp0=Cudd_bddAnd(env->mgr,vari,varj);
      Cudd_Ref(tmp0);//added
      tmp1=Cudd_Not(tmp0);
      Cudd_Ref(tmp1);//added
      tmp2=Cudd_bddAnd(env->mgr,cons,tmp1);
      Cudd_Ref(tmp2);
      cons=tmp2;
      Cudd_Ref(cons);//added
    }
  }
  varlast=Cudd_bddIthVar(env->mgr,var.firstBoolVar+var.nVal-1);
  tmpor=Cudd_bddOr(env->mgr,or,varlast);
  Cudd_Ref(tmpor);
  Cudd_RecursiveDeref(env->mgr,or);
  tmp1=Cudd_bddAnd(env->mgr,cons,tmpor);
  Cudd_Ref(tmp1);
  Cudd_RecursiveDeref(env->mgr,cons);
  cons=tmp1;

  return((long)cons);
}

void abd_clist_to_pllist(explan_t *mpa, prolog_term out)
{
  prolog_term tail,head,var,val;
  assign a;
  if (mpa==NULL)
    c2p_nil(out);
  else
  {
    c2p_list(out);
    head=p2p_car(out);
    tail=p2p_cdr(out);
    abd_clist_to_pllist(mpa->next, tail);
    a=mpa->a;
    c2p_functor("-", 2, head);
    var=p2p_arg(head,1);
    val=p2p_arg(head,2);
    c2p_int(a.var,var);
    c2p_int(a.val,val);
  }
}

void clist_to_pllist(explan_t *mpa, environment * env, prolog_term out)
{
  prolog_term tail,head,var,val;
  assign a;
  int value,bvar, mvari, mval;
  variable mvar;

  if (mpa==NULL)
    c2p_nil(out);
  else
  {

    for (; mpa; mpa=mpa->next)
    {
      a=mpa->a;
      bvar=a.var;
      value=a.val;
      if (value)
      {
        c2p_list(out);
        mvari=env->bVar2mVar[bvar];
        mvar=env->vars[mvari];
        mval=a.var-mvar.firstBoolVar+1;

        head=p2p_car(out);
        tail=p2p_cdr(out);
        c2p_functor("-", 2, head);
        var=p2p_arg(head,1);
        val=p2p_arg(head,2);
        c2p_int(mvari,var);
        c2p_int(mval,val);
        out=tail;
      }
    }
    c2p_nil(out);
  }
}

void vit_clist_to_pllist(explan_t *mpa, environment * env, prolog_term out)
{
  prolog_term tail,head,var,val;
  assign a;
  int value,bvar, mvari, mval,nVars,i,*assignments;
  variable mvar;

  if (mpa==NULL)
    c2p_nil(out);
  else
  {
    nVars=env->nVars;
    assignments=malloc(nVars*sizeof(int));
    for (i=0;i<nVars;i++)
      assignments[i]=-1;

    for (; mpa; mpa=mpa->next)
    {
      a=mpa->a;
      bvar=a.var;
      value=a.val;
      mvari=env->bVar2mVar[bvar];
      mvar=env->vars[mvari];
      if (value)
      {
        mval=a.var-mvar.firstBoolVar;
        assignments[mvari]=mval;
      }
      else
      {
        assignments[mvari]=env->vars[mvari].nVal-1;
      }
    }

    for (i=0;i<nVars;i++)
    {
      if (assignments[i]!=-1)
      {
        c2p_list(out);
        head=p2p_car(out);
        tail=p2p_cdr(out);
        c2p_functor("-", 2, head);
        var=p2p_arg(head,1);
        val=p2p_arg(head,2);
        c2p_int(i,var);
        c2p_int(assignments[i],val);
        out=tail;
      }
    }
    c2p_nil(out);
  }
}

double Prob(DdNode *node, environment * env, tablerow * table)
/* compute the probability of the expression rooted at node.
table is used to store nodeB for which the probability has alread been computed
so that it is not recomputed
 */
{
  int index;
  double res;
  double p,pt,pf,BChild0,BChild1;
  double * value_p;
  DdNode *nodekey,*T,*F;
  //comp=(comp && !comp_par) ||(!comp && comp_par);
  if (Cudd_IsConstant(node))
  {
      return 1.0;
  }
  else
  {
    nodekey=Cudd_Regular(node);
    value_p=get_value(table,nodekey);
    if (value_p!=NULL)
        return *value_p;
    else
    {
      index=Cudd_NodeReadIndex(node);  //Returns the index of the node. The node pointer can be either regular or complemented.
      //The index field holds the name of the variable that labels the node. The index of a variable is a permanent attribute that reflects the order of creation.
      p=env->probs[index];
      T = Cudd_T(node);
      F = Cudd_E(node);
      pf=Prob(F,env,table);
      pt=Prob(T,env,table);
      if (Cudd_IsComplement(F))
        pf=1.0-pf;

      BChild0=pf*(1-p);
      BChild1=pt*p;
      res=BChild0+BChild1;
      add_node(table,nodekey,res);
      return res;
    }
  }
}

prob_abd_expl abd_Prob(DdNode *node, environment * env,
  expltablerow * expltable, tablerow * table,
  int comp_par)
/* compute the probability of the expression rooted at node.
table is used to store nodeB for which the probability has alread been computed
so that it is not recomputed
 */
{
  int index,comp,pos;
  double p,p0,p1;
  DdNode *nodekey,*T,*F;
  prob_abd_expl deltat,deltaf,delta,*deltaptr;
  assign assignment;
  explan_t * mpa0,* mpa1,* mpa;

  comp=Cudd_IsComplement(node);
  comp=(comp && !comp_par) ||(!comp && comp_par);
  index=Cudd_NodeReadIndex(node);
  pos=Cudd_ReadPerm(env->mgr,index);
  if (pos>=env->n_abd_boolVars)
  {
    p1=Prob(node,env,table);
    if (comp)
      p1= 1.0-p1;

    delta.prob=p1;
    delta.mpa=NULL;
    return delta;
  }
  else
  {
    nodekey=Cudd_Regular(node);
    deltaptr=expl_get_value(expltable,nodekey,comp);
    if (deltaptr!=NULL)
    {
      return *deltaptr;
    }
    else
    {
      T = Cudd_T(node);
      F = Cudd_E(node);
      deltaf=abd_Prob(F,env,expltable,table,comp);
      deltat=abd_Prob(T,env,expltable,table,comp);
      p=env->probs[index];
      if (p==1.0)
      {
        p0=deltaf.prob;
        p1=deltat.prob;
      }
      else
      {
        p0=deltaf.prob*(1-p);
        p1=deltat.prob*p;
      }

      mpa0=deltaf.mpa;
      mpa1=deltat.mpa;

      if (p1>p0)
      {
        assignment.var=env->bVar2mVar[index];
        assignment.val=1;
        mpa=insert_ass(assignment,mpa1);
        delta.prob=p1;
        delta.mpa=mpa;
      }
      else
      {
        assignment.var=env->bVar2mVar[index];
        assignment.val=0;
        mpa=insert_ass(assignment,mpa0);
        delta.prob=p0;
        delta.mpa=mpa;
      }
      expl_add_node(expltable,nodekey,comp,delta);
      return delta;
    }
  }
}

prob_abd_expl map_Prob(DdNode *node, environment * env,
  expltablerow * maptable, tablerow * table,
  int comp_par)
/* compute the probability of the expression rooted at node.
table is used to store nodeB for which the probability has alread been computed
so that it is not recomputed
 */
{
  int index,comp,pos;
  double p,p0,p1;
  DdNode *nodekey,*T,*F;
  prob_abd_expl deltat,deltaf,delta,*deltaptr;
  assign assignment;
  explan_t * mpa0,* mpa1,* mpa;


  index=Cudd_NodeReadIndex(node);
  pos=Cudd_ReadPerm(env->mgr,index);
  comp=Cudd_IsComplement(node);
  comp=(comp && !comp_par) ||(!comp && comp_par);

  if (pos>=env->n_abd_boolVars)
  {
    p1=Prob(node,env,table);
    if (comp)
      p1= 1.0-p1;
    delta.prob=p1;
    delta.mpa=NULL;


    return delta;
  }
  else
  {
    nodekey=Cudd_Regular(node);
    deltaptr=expl_get_value(maptable,nodekey,comp);
    if (deltaptr!=NULL)
    {
      return *deltaptr;
    }
    p=env->probs[index];
    T = Cudd_T(node);
    F = Cudd_E(node);
    deltaf=map_Prob(F,env,maptable,table,comp);
    deltat=map_Prob(T,env,maptable,table,comp);

    p0=deltaf.prob;
    mpa0=deltaf.mpa;

    p1=deltat.prob*p;
    mpa1=deltat.mpa;

    if (p1>p0)
    {
      assignment.var=index;
      assignment.val=1;
      mpa=insert_ass(assignment,mpa1);
      delta.prob=p1;
      delta.mpa=mpa;
    }
    else
    {
      assignment.var=index;
      assignment.val=0;
      mpa=insert_ass(assignment,mpa0);
      delta.prob=p0;
      delta.mpa=mpa;
    }
    expl_add_node(maptable,nodekey,comp,delta);
    return delta;
  }

}
prob_abd_expl vit_Prob(DdNode *node, environment * env,
  expltablerow * expltable, tablerow * table,
  int comp_par)
/* compute the probability of the expression rooted at node.
table is used to store nodeB for which the probability has alread been computed
so that it is not recomputed
 */
{
  int index,comp;
  double p,p0,p1;
  DdNode *nodekey,*T,*F;
  prob_abd_expl deltat,deltaf,delta,*deltaptr;
  assign assignment;
  explan_t * mpa0,* mpa1,* mpa;

  comp=Cudd_IsComplement(node);
  comp=(comp && !comp_par) ||(!comp && comp_par);
  index=Cudd_NodeReadIndex(node);
  if (Cudd_IsConstant(node))
  {

    if (comp)
      p1= 0.0;
    else
      p1= 1.0;

    delta.prob=p1;
    delta.mpa=NULL;

    return delta;
  }
  else
  {
    nodekey=Cudd_Regular(node);
    deltaptr=expl_get_value(expltable,nodekey,comp);
    if (deltaptr!=NULL)
    {
      return *deltaptr;
    }
    else
    {

      T = Cudd_T(node);
      F = Cudd_E(node);
      deltaf=vit_Prob(F,env,expltable,table,comp);
      deltat=vit_Prob(T,env,expltable,table,comp);
      p=env->probs[index];


      p0=deltaf.prob*(1-p);
      p1=deltat.prob*p;

      mpa0=deltaf.mpa;
      mpa1=deltat.mpa;

      if (p1>p0)
      {
        assignment.var=index;
        assignment.val=1;
        mpa=insert_ass(assignment,mpa1);
        delta.prob=p1;
        delta.mpa=mpa;
      }
      else
      {
        assignment.var=index;
        assignment.val=0;
        mpa=insert_ass(assignment,mpa0);
        delta.prob=p0;
        delta.mpa=mpa;
      }
      expl_add_node(expltable,nodekey,comp,delta);
      return delta;
    }
  }
}

explan_t * insert_ass(assign assignment,explan_t * head)
{
  explan_t * newhead;
  newhead=malloc(sizeof(explan_t));
  newhead->a=assignment;
  newhead->next=duplicate(head);
  return newhead;
}

explan_t * duplicate(explan_t * head)
{
  explan_t * newhead;
  if (head)
  {
    newhead=malloc(sizeof(explan_t));
    newhead->a=head->a;
    newhead->next=duplicate(head->next);
  }
  else
    newhead=NULL;
  return newhead;
}

void free_list(explan_t * head)
{
  if (head)
  {
    free_list(head->next);
    free(head);
  }
}
long add_var_pl(long env_int, prolog_term probTerm, int nRule)
{
  variable * v;
  int i,nRules;
  int lenProbs;
  double p,p0;
  environment * env;

  env=(environment *)env_int;
  env->nVars=env->nVars+1;
  env->vars=(variable *) realloc(env->vars,env->nVars * sizeof(variable));

  v=&env->vars[env->nVars-1];
  v->abducible=0;
  v->query=0;
  
  v->nRule=nRule;
 
  lenProbs=length(probTerm);
  v->nVal=lenProbs;
  nRules=env->nRules;
  if (v->nRule>=nRules)
  {
    env->rules=(int *)  realloc(env->rules,((v->nRule+1)* sizeof(int)));
    for (i=nRules;i<v->nRule;i++)
      env->rules[i]=0;
    env->rules[v->nRule]=lenProbs;
    env->nRules=v->nRule+1;
  }
  v->firstBoolVar=env->boolVars;
  env->probs=(double *) realloc(env->probs,(((env->boolVars+v->nVal-1)* sizeof(double))));
  env->bVar2mVar=(int *) realloc(env->bVar2mVar,((env->boolVars+v->nVal-1)* sizeof(int)));

  p0=1;
  for (i=0;i<v->nVal-1;i++)
  {
    p=p2c_float(p2p_car(probTerm));
    env->bVar2mVar[env->boolVars+i]=env->nVars-1;
    env->probs[env->boolVars+i]=p/p0;
    p0=p0*(1-p/p0);
    probTerm=p2p_cdr(probTerm);
  }
  env->boolVars=env->boolVars+v->nVal-1;
  env->rules[v->nRule]= v->nVal;

  return(env->nVars-1);
}

int length(prolog_term list)
{
  int l=0;

  while (!is_nil(list))
  {
    list=p2p_cdr(list);
    l++;
  }
  return l;
}

int add_query_var_pl(long env_int, prolog_term probTerm, int nRule)
{
  variable * v;
  int i,nRules;
  int lenProbs;
  double p;
  environment * env;

  env=(environment *)env_int;
  env->nVars=env->nVars+1;
  env->n_abd++;
  env->vars=(variable *) realloc(env->vars,env->nVars * sizeof(variable));

  v=&env->vars[env->nVars-1];
  v->query=1;
  v->abducible=0;

  lenProbs=length(probTerm);
  v->nVal=lenProbs;

  v->nRule=nRule;

  nRules=env->nRules;
  if (v->nRule>=nRules)
  {
    env->rules=(int *)  realloc(env->rules,((v->nRule+1)* sizeof(int)));
    for (i=nRules;i<v->nRule;i++)
      env->rules[i]=0;
    env->rules[v->nRule]=lenProbs;
    env->nRules=v->nRule+1;
  }

  env->n_abd_boolVars=env->n_abd_boolVars+v->nVal;
  v->firstBoolVar=env->boolVars;
  env->probs=(double *) realloc(env->probs,(((env->boolVars+v->nVal)* sizeof(double))));
  env->bVar2mVar=(int *) realloc(env->bVar2mVar,((env->boolVars+v->nVal)* sizeof(int)));

  for (i=0;i<v->nVal;i++)
  {
    p=p2c_float(p2p_car(probTerm));
    probTerm=p2p_cdr(probTerm);
    env->bVar2mVar[env->boolVars+i]=env->nVars-1;
    env->probs[env->boolVars+i]=p;
  }
  env->boolVars=env->boolVars+v->nVal;
  env->rules[v->nRule]= v->nVal;

  return(env->nVars-1);
}

int add_abd_var_pl(long env_int, prolog_term probTerm, int nRule)
{
  variable * v;
  int i,nRules;
  double p,p0;
  environment * env;
  int lenProbs;

  env=(environment *)env_int;

  env->nVars=env->nVars+1;
  env->vars=(variable *) realloc(env->vars,env->nVars * sizeof(variable));

  env->n_abd=env->n_abd+1;

  v=&env->vars[env->nVars-1];

  v->abducible=1;
  v->query=0;
  lenProbs=length(probTerm);

  v->nVal=lenProbs;
  v->nRule=nRule;

  nRules=env->nRules;
  if (v->nRule>=nRules)
  {
    env->rules=(int *)  realloc(env->rules,((v->nRule+1)* sizeof(int)));
    for (i=nRules;i<v->nRule;i++)
      env->rules[i]=0;
    env->rules[v->nRule]=lenProbs;
    env->nRules=v->nRule+1;
  }
  env->n_abd_boolVars=env->n_abd_boolVars+v->nVal-1;

  v->firstBoolVar=env->boolVars;
  env->probs=(double *) realloc(env->probs,(((env->boolVars+v->nVal-1)* sizeof(double))));
  env->bVar2mVar=(int *) realloc(env->bVar2mVar,((env->boolVars+v->nVal-1)* sizeof(int)));

  p0=1;
  for (i=0;i<v->nVal-1;i++)
  {
    p=p2c_float(p2p_car(probTerm));
    probTerm=p2p_cdr(probTerm);
    env->bVar2mVar[env->boolVars+i]=env->nVars-1;
    env->probs[env->boolVars+i]=p/p0;
    p0=p0*(1-p/p0);
  }
  env->boolVars=env->boolVars+v->nVal-1;
  env->rules[v->nRule]= v->nVal;
  return(env->nVars-1);

}

long equality_pl(long env_int, int varIndex, int value)
{
  int i;
  variable v;
  DdNode * node, * tmp,*var;
  environment * env;

  env=(environment *)env_int;
  v=env->vars[varIndex];
  i=v.firstBoolVar;
  tmp=Cudd_ReadOne(env->mgr);
  Cudd_Ref(tmp);
  node=NULL;
  if (v.query)
  {
    var=Cudd_bddIthVar(env->mgr,v.firstBoolVar+value);
    node=Cudd_bddAnd(env->mgr,tmp,var);
    Cudd_Ref(node);
  }
  else
  {
    for (i=v.firstBoolVar;i<v.firstBoolVar+value;i++)
    {
      var=Cudd_bddIthVar(env->mgr,i);
      node=Cudd_bddAnd(env->mgr,tmp,Cudd_Not(var));
      Cudd_Ref(node);
      Cudd_RecursiveDeref(env->mgr,tmp);
      tmp=node;
    }
    if (!(value==v.nVal-1))
    {
      var=Cudd_bddIthVar(env->mgr,v.firstBoolVar+value);
      node=Cudd_bddAnd(env->mgr,tmp,var);
      Cudd_Ref(node);
      Cudd_RecursiveDeref(env->mgr,tmp);
    }
  }
  return((long) node);
}

long one_pl(long env_int)
{
  DdNode * node;
  environment *env;

  env=(environment *)env_int;

  node =  Cudd_ReadOne(env->mgr);
  Cudd_Ref(node);
  return ((long)node);

}

long zero_pl(long env_int)
{
  DdNode * node;
  environment *env;

  env=(environment *)env_int;

  node = Cudd_ReadLogicZero(env->mgr);
  Cudd_Ref(node);
  return((long)node);
}
long bdd_not_pl(long env_int, long node_int)
{
  DdNode * node;

  node=(DdNode*)node_int;
  node=Cudd_Not(node);
  return((long)node);
}

long and_pl(long env_int, long node1_int, long node2_int)
{
  DdNode * node1, *node2,*nodeout;
  environment *env;

  env=(environment *)env_int;
  
  node1=(DdNode *)node1_int;
  node2=(DdNode *)node2_int;
  nodeout=Cudd_bddAnd(env->mgr,node1,node2);
  Cudd_Ref(nodeout);
  return ((long) nodeout);
}

long or_pl(long env_int, long node1_int, long node2_int)
{
  DdNode * node1, *node2,*nodeout;
  environment *env;

  env=(environment *)env_int;


  node1=(DdNode *)node1_int;
  node2=(DdNode *)node2_int;

  nodeout=Cudd_bddOr(env->mgr,node1,node2);
  Cudd_Ref(nodeout);
  return ((long) nodeout);
}

/*
static int garbage_collect(void)
{
  YAP_Term arg1,arg2,out;
  YAP_Int nodes,clearCache;

  arg1=YAP_ARG1;
  arg2=YAP_ARG2;
  clearCache=YAP_IntOfTerm(arg1);
  nodes=(YAP_Int)cuddGarbageCollect(mgr_ex[ex],clearCache);
  out=YAP_MkIntTerm(nodes);
  return(YAP_Unify(out,arg2));
}

static int bdd_to_add(void)
{
  YAP_Term arg1,arg2,out;
  DdNode * node1,*node2;

  arg1=YAP_ARG1;
  arg2=YAP_ARG2;
  node1=(DdNode *)YAP_IntOfTerm(arg1);
  node2= Cudd_BddToAdd(mgr_ex[ex],node1);
  out=YAP_MkIntTerm((YAP_Int) node2);
  return(YAP_Unify(out,arg2));
}
*/

void create_dot_pl(long env_int, long node_int, char * filename)
{
  DdNode * node;
  environment *env;
  FILE * file;

  env=(environment *)env_int;
  node=(DdNode *)node_int;

  file = open_file(filename, "w");
  write_dot(env,node,file);
  fclose(file);
}

void create_dot_string_pl(long env_int, long node_int, char ** string)
{
  DdNode * node;
  environment *env;
  FILE * file;
  char *buffer=NULL;
  env=(environment *)env_int;
  node=(DdNode *)node_int;

#ifndef _WIN32
  file=tmpfile();
#else
  char filename[MAX_PATH];
  GetTempFileName(".","temp",0,filename);
  file = fopen(filename,"w+bTD");
#endif
  if (file==NULL) {perror("Error in temporary file opening");}
  write_dot(env,node,file);

  if (fseek(file, 0L, SEEK_END) == 0) {
    /* Get the size of the file. */
    long bufsize = ftell(file);
    if (bufsize == -1) { perror("Error in getting the size of the temporary file");}

      /* Allocate our buffer to that size. */
        buffer = malloc(sizeof(char) * (bufsize + 1));

        /* Go back to the start of the file. */
        if (fseek(file, 0L, SEEK_SET) != 0) { perror("Error going back to the start of the file");}

        /* Read the entire file into memory. */
        size_t newLen = fread(buffer, sizeof(char), bufsize, file);
        if ( ferror( file ) != 0 ) {
            perror("Error reading file");
        } else {
            buffer[newLen++] = '\0'; /* Just to be safe. */
        }
  }
  fclose(file);
  *string=buffer;
}

void write_dot(environment * env, DdNode * bdd, FILE * file)
{
  char * onames[]={"Out"};
  char ** inames;
  int i,b,index,nv;
  variable v;
  char numberVar[11],numberBit[11];
  inames= (char **) malloc(sizeof(char *)*(env->boolVars));
  index=0;
  for (i=0;i<env->nVars;i++)
  {
    v=env->vars[i];
      if (v.query)
        nv=v.nVal;
      else
        nv=v.nVal-1;
    for (b=0;b<nv;b++)
    {
      inames[b+index]=(char *) malloc(sizeof(char)*20);
      strcpy(inames[b+index],"X");
      sprintf(numberVar,"%d",i);
      strcat(inames[b+index],numberVar);
      strcat(inames[b+index],"_");
      sprintf(numberBit,"%d",b);
      strcat(inames[b+index],numberBit);
    }
    index=index+nv;
  }
  Cudd_DumpDot(env->mgr,1,&bdd,(const char * const *)inames,(const char * const *)onames,file);
  index=0;
  for (i=0;i<env->nVars;i++)
  {
    v=env->vars[i];
    if (v.query)
      nv=v.nVal;
    else
      nv=v.nVal-1;
    for (b=0;b<nv;b++)
    {
      free(inames[b+index]);
    }
    index=index+nv;
  }
  free(inames);
}

/*
static int rec_deref(void)
{
  YAP_Term arg1;
  DdNode * node;

  arg1=YAP_ARG1;
  node=(DdNode *) YAP_IntOfTerm(arg1);
  Cudd_RecursiveDeref(mgr_ex[ex], node);
  return 1;
}

*/

double ProbPath(example_data * ex_d,DdNode *node, int nex)
{
  int index,mVarIndex,pos,position;//,boolVarIndex;
  variable v;
  double res;
  double p,pt,pf,BChild0e,BChild1e,BChild0o,BChild1o,e0,e1;
  double *value_p, * value_p_e, *value_p_o,** eta_rule;
  DdNode *nodekey,*T,*F;

  // printf("node %p comp %d\n",node,comp );
  if (Cudd_IsConstant(node))
  {
    // printf("constant\n");
    return 1.0;
  }
  else
  {
    nodekey=Cudd_Regular(node);
    value_p=get_value(ex_d->nodesB,nodekey);
    if (value_p!=NULL)
    {
      // printf("found %f\n", *value_p);
      return *value_p;
    }
    else
    {
      index=Cudd_NodeReadIndex(node);
      p=ex_d->env[nex].probs[index];
      T = Cudd_T(node);
      F = Cudd_E(node);
      pf=ProbPath(ex_d,F,nex);
      pt=ProbPath(ex_d,T,nex);
      // printf("pt %f pf %f\n",pt,pf );
      if (Cudd_IsComplement(F))
        pf=1.0-pf;
      // printf("pt %f pf %f\n",pt,pf );

      BChild0e=pf*(1-p);
      BChild0o=(1-pf)*(1-p);
      BChild1e=pt*p;
      BChild1o=(1-pt)*p;
      value_p_e=get_value(ex_d->nodesFE,nodekey);
      value_p_o=get_value(ex_d->nodesFO,nodekey);
      e0 = (*value_p_e)*BChild0e+(*value_p_o)*BChild0o;
      e1 = (*value_p_e)*BChild1e+(*value_p_o)*BChild1o;
    //  printf("e node %p %f %f %f %f\n",node,*value_p_e,*value_p_o,e0,e1 );
      mVarIndex=ex_d->env[nex].bVar2mVar[index];
      v=ex_d->env[nex].vars[mVarIndex];
      pos=index-v.firstBoolVar;
      if (ex_d->tunable_rules[v.nRule])
      {
        eta_rule=ex_d->eta_temp[v.nRule];
        eta_rule[pos][0]=eta_rule[pos][0]+e0;
        eta_rule[pos][1]=eta_rule[pos][1]+e1;
      }
      res=BChild0e+BChild1e;
      add_node(ex_d->nodesB,nodekey,res);
      position=Cudd_ReadPerm(ex_d->env[nex].mgr,index);
      position=position+1;
//      boolVarIndex=Cudd_ReadInvPerm(ex_d->env[nex].mgr,position);//Returns the index of the variable currently in the i-th position of the order.
      if (position<ex_d->env[nex].boolVars)
      {
        ex_d->sigma[position]=ex_d->sigma[position]+e0+e1;
      }
      if(!Cudd_IsConstant(T))
      {
        index=Cudd_NodeReadIndex(T);
        position=Cudd_ReadPerm(ex_d->env[nex].mgr,index);
        ex_d->sigma[position]=ex_d->sigma[position]-e1;
      }

      if(!Cudd_IsConstant(F))
      {
        index=Cudd_NodeReadIndex(F);
        position=Cudd_ReadPerm(ex_d->env[nex].mgr,index);
        ex_d->sigma[position]=ex_d->sigma[position]-e0;
      }

      return res;
    }
  }
}




void Forward(example_data * ex_d,DdNode *root, int nex)
{
  DdNode *** nodesToVisit;
  int * NnodesToVisit,comp;
  double FrootE,FrootO;

  environment env;
  int i,j;
  env=ex_d->env[nex];
  comp=Cudd_IsComplement(root);
  if (comp)
  {
    FrootE=0.0;
    FrootO=1.0;
  }
  else
  {
    FrootE=1.0;
    FrootO=0.0;
  }
  add_node(ex_d->nodesFE,Cudd_Regular(root),FrootE);
  add_node(ex_d->nodesFO,Cudd_Regular(root),FrootO);
  if (env.boolVars)
  {
    nodesToVisit= (DdNode ***)malloc(sizeof(DdNode **)* env.boolVars);
    NnodesToVisit= (int *)malloc(sizeof(int)* env.boolVars);
    nodesToVisit[0]=(DdNode **)malloc(sizeof(DdNode *));
    nodesToVisit[0][0]=root;
    NnodesToVisit[0]=1;
    for(i=1;i<env.boolVars;i++)
    {
      nodesToVisit[i]=NULL;
      NnodesToVisit[i]=0;
    }
    for(i=0;i<env.boolVars;i++)
    {
      for(j=0;j<NnodesToVisit[i];j++)
      UpdateForward(ex_d,nodesToVisit[i][j],nex,nodesToVisit,NnodesToVisit);
    }
    for(i=0;i<env.boolVars;i++)
    {
      free(nodesToVisit[i]);
    }
    free(nodesToVisit);
    free(NnodesToVisit);
  }
}

void UpdateForward(example_data *ex_d,DdNode *node, int nex,
  DdNode *** nodesToVisit, int * NnodesToVisit)
{
  int index,position;
  DdNode *T,*E,*nodereg;
  double *value_p_E,*value_p_O,*value_p_T_E,*value_p_T_O,
    *value_p_F_E,*value_p_F_O,p,value_par_E,value_par_O;

// printf("F node %p comp %d\n",node,Cudd_IsComplement(node) );
  if (Cudd_IsConstant(node))
  {
    return;
  }
  else
  {
    index=Cudd_NodeReadIndex(node);
    p=ex_d->env[nex].probs[index];
    nodereg=Cudd_Regular(node);
    value_p_E=get_value(ex_d->nodesFE,nodereg);
    value_p_O=get_value(ex_d->nodesFO,nodereg);
    if (value_p_E== NULL)
    {
      printf("Error\n");
      return;
    }
    else
    {
      T = Cudd_T(node);
      E = Cudd_E(node);
      if (!Cudd_IsConstant(T))
      {
        value_p_T_E=get_value(ex_d->nodesFE,T);
        value_p_T_O=get_value(ex_d->nodesFO,T);
        if (value_p_T_E!= NULL)
        {
           *value_p_T_E= *value_p_T_E+*value_p_E*p;
           *value_p_T_O= *value_p_T_O+*value_p_O*p;
          // printf("update f t %p %f %f %f %f\n",T,*value_p_T_E,
          //  *value_p_E*p,*value_p_T_O,*value_p_O*p);
        }
        else
        {
          // printf("new f t %p %f %f \n",T,*value_p_E*p,*value_p_O*p );

          add_or_replace_node(ex_d->nodesFE,Cudd_Regular(T),*value_p_E*p);
          add_or_replace_node(ex_d->nodesFO,Cudd_Regular(T),*value_p_O*p);
          index=Cudd_NodeReadIndex(T);
          position=Cudd_ReadPerm(ex_d->env[nex].mgr,index);
          nodesToVisit[position]=(DdNode **)realloc(nodesToVisit[position],
	    (NnodesToVisit[position]+1)* sizeof(DdNode *));
          nodesToVisit[position][NnodesToVisit[position]]=T;
          NnodesToVisit[position]=NnodesToVisit[position]+1;
        }
      }
      if (!Cudd_IsConstant(E))
      {
        value_p_F_E=get_value(ex_d->nodesFE,Cudd_Regular(E));
        value_p_F_O=get_value(ex_d->nodesFO,Cudd_Regular(E));
        // if (Cudd_IsComplement(E))
        //   value_par=1 - *value_p;
        // else
        //   value_par= *value_p;
        // if (Cudd_IsComplement(E))
        //   p=1 -p;
        if (Cudd_IsComplement(E))
        {
          value_par_E= *value_p_O;
          value_par_O= *value_p_E;
        }
        else
        {
          value_par_E= *value_p_E;
          value_par_O= *value_p_O;
        }
        // printf("f child %d %f %f\n",Cudd_IsComplement(E),value_par_E,value_par_O );

        if (value_p_F_E!= NULL)
        {

          *value_p_F_E= *value_p_F_E+value_par_E*(1-p);
          *value_p_F_O= *value_p_F_O+value_par_O*(1-p);
          // printf("update f f %p %f %f %f %f\n",E,*value_p_F_E, value_par_E*(1-p),
        // *value_p_F_O, value_par_O*(1-p));
        }
        else
        {
                              // printf("new f f %p %f\n",E,value_par_E*(1-p) );

          add_or_replace_node(ex_d->nodesFE,Cudd_Regular(E),value_par_E*(1-p));
          add_or_replace_node(ex_d->nodesFO,Cudd_Regular(E),value_par_O*(1-p));
          index=Cudd_NodeReadIndex(E);
          position=Cudd_ReadPerm(ex_d->env[nex].mgr,index);
          nodesToVisit[position]=(DdNode **)realloc(nodesToVisit[position],
	    (NnodesToVisit[position]+1)* sizeof(DdNode *));
          nodesToVisit[position][NnodesToVisit[position]]=E;
          NnodesToVisit[position]=NnodesToVisit[position]+1;
        }
      }
      return;
    }
  }
}




double GetOutsideExpe(example_data * ex_d,DdNode *root,double ex_prob, int nex)
{
  int i,j,mVarIndex,bVarIndex,firstBoolVarOfRule,nRule;
  double **eta_rule;
  double theta,rootProb, T=0;


  ex_d->sigma=(double *)malloc(ex_d->env[nex].boolVars * sizeof(double));

  for (j=0; j<ex_d->env[nex].boolVars; j++)
  {
    ex_d->sigma[j]=0;
  }
  for (j=0; j<ex_d->nRules; j++)
  {
    if (ex_d->tunable_rules[j])
      for (i=0; i<ex_d->rules[j]-1; i++)
      {
        ex_d->eta_temp[j][i][0]=0;
        ex_d->eta_temp[j][i][1]=0;
      }
  }
  rootProb=ProbPath(ex_d,root,nex);
  if (Cudd_IsComplement(root))
    rootProb=1.0-rootProb;
  if (rootProb>0.0)
  {
    for (j=0; j<ex_d->env[nex].boolVars; j++)
    {
      T += ex_d->sigma[j];
      bVarIndex=Cudd_ReadInvPerm(ex_d->env[nex].mgr,j);
      if (bVarIndex==-1)
      {
        bVarIndex=j;
      }

      mVarIndex=ex_d->env[nex].bVar2mVar[bVarIndex];

      firstBoolVarOfRule=ex_d->env[nex].vars[mVarIndex].firstBoolVar;
      i=bVarIndex-firstBoolVarOfRule;
      theta=ex_d->env[nex].probs[bVarIndex];
      nRule=ex_d->env[nex].vars[mVarIndex].nRule;
      if (ex_d->tunable_rules[nRule])
      {
        eta_rule=ex_d->eta_temp[nRule];
        eta_rule[i][0]=eta_rule[i][0]+T*(1-theta);
        eta_rule[i][1]=eta_rule[i][1]+T*theta;
      }
    }

    for (j=0; j<ex_d->nRules; j++)
    {
      if (ex_d->tunable_rules[j])
        for (i=0; i<ex_d->rules[j]-1; i++)
        {
          ex_d->eta[j][i][0]=ex_d->eta[j][i][0]+
      ex_d->eta_temp[j][i][0]*ex_prob/rootProb;
          ex_d->eta[j][i][1]=ex_d->eta[j][i][1]+
      ex_d->eta_temp[j][i][1]*ex_prob/rootProb;
        }
    }
  }
  free(ex_d->sigma);
  return rootProb;
}


void Maximization(example_data * ex_d)
{
  int r,i,j,e;
  double sum=0;
  double *probs_rule,**eta_rule;

  for (r=0;r<ex_d->nRules;r++)
  {
    if (ex_d->tunable_rules[r])
    {
      eta_rule=ex_d->eta[r];
      for (i=0;i<ex_d->rules[r]-1;i++)
      {
        sum=(eta_rule[i][0]+eta_rule[i][1]);
        if (sum==0.0)
        {
          ex_d->arrayprob[r][i]=0;
        }
        else
          ex_d->arrayprob[r][i]=eta_rule[i][1]/sum;
      }
    }
  }

  for(e=0;e<ex_d->ex;e++)
  {
    for (j=0;j<ex_d->env[e].nVars;j++)
    {
      r=ex_d->env[e].vars[j].nRule;
      if (ex_d->tunable_rules[r])
      {
        probs_rule=ex_d->arrayprob[r];
        for(i=0;i<ex_d->rules[r]-1;i++)
        {
          ex_d->env[e].probs[ex_d->env[e].vars[j].firstBoolVar+i]=probs_rule[i];
        }
      }
    }
  }
}


void init_par(example_data * ex_d, prolog_term ruleHeadsTerm)
{
  double * theta,p0;
  double pmass,par;
  double **Theta_rules;
  double ***eta;
  double ***eta_temp;
  int i,j,e,rule;
  prolog_term head,p;
  size_t nHeads,nRules;
  int *rules, *tun_rules;

  nRules=length(ruleHeadsTerm);

  ex_d->nRules=nRules;
  ex_d->rules= (int *) malloc(nRules * sizeof(int));
  rules=ex_d->rules;
  ex_d->tunable_rules= (int *) malloc(nRules * sizeof(int));
  tun_rules=ex_d->tunable_rules;
  ex_d->eta= (double ***) malloc(nRules * sizeof(double **));
  eta=ex_d->eta;
  ex_d->eta_temp= (double ***) malloc(nRules * sizeof(double **));
  eta_temp=ex_d->eta_temp;
  ex_d->nodes_probs=NULL;
  ex_d->arrayprob=(double **) malloc(nRules * sizeof(double *));

  Theta_rules=(double **)malloc(nRules *sizeof(double *));

  for (j=0;j<nRules;j++)
  {
    head=p2p_car(ruleHeadsTerm);
    ruleHeadsTerm=p2p_cdr(ruleHeadsTerm);
    pmass=0;
    if (is_list(head)) 
    { 
      nHeads=length(head);
      if (nHeads==1) // fixed parameters
      {
        head=p2p_car(head);
        nHeads=length(head);
        tun_rules[j]=0;
      }
      else // initial parameters
        tun_rules[j]=1;
        
      Theta_rules[j]=(double *)malloc(nHeads*sizeof(double));
      theta=Theta_rules[j];
      rules[j]=nHeads,
      ex_d->arrayprob[j]= (double *) malloc((nHeads-1)*sizeof(double));
      eta[j]= (double **) malloc((nHeads-1)*sizeof(double *));
      eta_temp[j]= (double **) malloc((nHeads-1)*sizeof(double *));

      for (i=0;i<nHeads-1;i++)
      {
        p=p2p_car(head);
        par=p2c_float(p);
        head=p2p_cdr(head);
        eta[j][i]=(double *) malloc(2*sizeof(double));
        eta_temp[j][i]=(double *) malloc(2*sizeof(double));
        pmass=pmass+par;
        theta[i]=par;
        ex_d->arrayprob[j][i]=par;
      }
      theta[nHeads-1]=1-pmass;
    }
    else
    {
      rules[j]=p2c_int(head);
      nHeads=rules[j];
      
      Theta_rules[j]=(double *)malloc(nHeads*sizeof(double));
      theta=Theta_rules[j];
      ex_d->arrayprob[j]= (double *) malloc((nHeads-1)*sizeof(double));
      
      eta[j]= (double **) malloc((nHeads-1)*sizeof(double *));
      eta_temp[j]= (double **) malloc((nHeads-1)*sizeof(double *));
      tun_rules[j]=1;
      for (i=0;i<rules[j]-1;i++)
      {
        eta[j][i]=(double *) malloc(2*sizeof(double));
        eta_temp[j][i]=(double *) malloc(2*sizeof(double));
      }
      if (ex_d->alpha==0.0)
      {
        for (i=0;i<rules[j]-1;i++)
        {
          par=uniform_sample_pl()*(1-pmass);
          pmass=pmass+par;
          theta[i]=par;
          ex_d->arrayprob[j][i]=par;
        }
        theta[nHeads-1]=1-pmass;
      }
      else
      {
        symmetric_dirichlet_sample_c(ex_d->alpha,rules[j],theta);
        for (i=0;i<rules[j]-1;i++)
          ex_d->arrayprob[j][i]=theta[i];
      }
    }
  }

  for(e=0;e<ex_d->ex;e++)
  {
    for (j=0; j<ex_d->env[e].nVars; j++)
    {
      rule=ex_d->env[e].vars[j].nRule;
      theta=Theta_rules[rule];
      p0=1;
      for (i=0; i<ex_d->env[e].vars[j].nVal-1;i++)
      {
        ex_d->env[e].probs[ex_d->env[e].vars[j].firstBoolVar+i]=theta[i]/p0;
        p0=p0*(1-theta[i]/p0);
      }
    }
  }
  for (j=0;j<ex_d->nRules;j++)
  {
    free(Theta_rules[j]);
  }
  free(Theta_rules);
}
void rand_seed_pl(int seed)
{
  srand((unsigned)seed);
}

double em_pl(example_data * ex_d_int, prolog_term ruleInfo, prolog_term nodesTerm, double ea,
  double er, int iter, prolog_term *out)
{
  prolog_term pterm,ruleTerm,tail,pair,compoundTerm,pars,pars_list,
    pars_rule, pars_rule_list, new_pars_rule, new_pars_list, new_tail,
    new_ex_probs_list, ex_probs_list, head, out_tail, out_tail_tail, ex_probs;
  DdNode * node1,**nodes_ex;
  int r,i,cycle;
  long iter1;
  size_t lenNodes;
  example_data * ex_d;


  double CLL0= -2.2*pow(10,10); //-inf
  double CLL1= -1.7*pow(10,8);  //+inf
  double p,p0,**eta_rule;
  double ratio,diff;


  ex_d=(example_data *) ex_d_int;

  init_par(ex_d,ruleInfo);


  lenNodes=length(nodesTerm);
  
  nodes_ex=(DdNode **)malloc(lenNodes*sizeof(DdNode*));
  ex_d->nodes_probs=(double *)malloc(lenNodes*sizeof(double));
  ex_d->example_prob=(double *)malloc(lenNodes*sizeof(double));

  for (i=0;i<lenNodes;i++)
  {
    pair=p2p_car(nodesTerm);
    nodesTerm=p2p_cdr(nodesTerm);
    head=p2p_car(pair);
    pair=p2p_cdr(pair);

    node1=(DdNode*)p2c_int(head);
    nodes_ex[i]=node1;
    head=p2p_car(pair);
    ex_d->example_prob[i]=p2c_float(head);
  }
  diff=CLL1-CLL0;
  ratio=diff/fabs(CLL0);
  if (iter==-1)
    iter1= 2147000000;
  else iter1=iter;

  cycle=0;
  while  ( (diff>ea) && (ratio>er) && (cycle<iter1) )
  {
    cycle++;
    for (r=0;r<ex_d->nRules;r++)
    {
      if (ex_d->tunable_rules[r])
        for (i=0;i<ex_d->rules[r]-1;i++)
        {
          eta_rule=ex_d->eta[r];
          eta_rule[i][0]=0;
          eta_rule[i][1]=0;
        }
    }
    CLL0 = CLL1;
    CLL1 = Expectation(ex_d,nodes_ex,lenNodes);
    Maximization(ex_d);
    diff=CLL1-CLL0;
    ratio=diff/fabs(CLL0);
  }
  *out=p2p_new();
  c2p_list(*out);
  pars_list=p2p_new();
  c2p_nil(pars_list);

  for (r=0; r<ex_d->nRules; r++)
  {
    pars_rule=p2p_new();
    c2p_nil(pars_rule);
    p0=1;
    for (i=0;i<ex_d->rules[r]-1;i++)
    {
      new_pars_rule=p2p_new();
      c2p_list(new_pars_rule);
      pterm=p2p_car(new_pars_rule);
      p=ex_d->arrayprob[r][i]*p0;
      c2p_float(p,pterm);
      new_tail=p2p_cdr(new_pars_rule);
      p2p_unify(new_tail,pars_rule);
      pars_rule=new_pars_rule;
      p0=p0*(1-ex_d->arrayprob[r][i]);
    }
    pars_rule_list=p2p_new();
    c2p_list(pars_rule_list);
    new_pars_rule=p2p_car(pars_rule_list);
    c2p_list(new_pars_rule);
    pterm=p2p_car(new_pars_rule);
    c2p_float(p0,pterm);
    new_tail=p2p_cdr(new_pars_rule);
    p2p_unify(new_tail,pars_rule);
    tail=p2p_cdr(pars_rule_list);
    c2p_nil(tail);

    new_pars_list=p2p_new();
    c2p_list(new_pars_list);
    compoundTerm=p2p_car(new_pars_list);
    c2p_list(compoundTerm);
    ruleTerm=p2p_car(compoundTerm);
    c2p_int(r,ruleTerm);
    tail=p2p_cdr(compoundTerm);
    p2p_unify(pars_rule_list,tail);
    new_tail=p2p_cdr(new_pars_list);
    p2p_unify(pars_list,new_tail);
    pars_list=new_pars_list;
  }
  pars=p2p_car(*out);
  p2p_unify(pars_list,pars);
  out_tail=p2p_cdr(*out);
  c2p_list(out_tail);
  out_tail_tail=p2p_cdr(out_tail);
  c2p_nil(out_tail_tail);
  ex_probs_list=p2p_new();
  c2p_nil(ex_probs_list);
  for (i=0;i<lenNodes;i++)
  {
    new_ex_probs_list=p2p_new();
    c2p_list(new_ex_probs_list);
    pterm=p2p_car(new_ex_probs_list);
    c2p_float(ex_d->nodes_probs[i],pterm);
    tail=p2p_cdr(new_ex_probs_list);
    p2p_unify(ex_probs_list,tail);
    ex_probs_list=new_ex_probs_list;
  }
  ex_probs=p2p_car(out_tail);
  p2p_unify(ex_probs,ex_probs_list);

  free(nodes_ex);
  free(ex_d->example_prob);
  free(ex_d->nodes_probs);
  return CLL1;
}

// double em_pl(example_data * ex_d_int, prolog_term ruleInfo, prolog_term nodesTerm, double ea,
//   double er, int iter, prolog_term *pars, prolog_term *exprobs)
// {
//   prolog_term pterm,ruleTerm,tail,pair,compoundTerm,pars_list,
//     pars_rule, pars_rule_list, new_pars_rule, new_pars_list, new_tail,
//     new_exprobs_list, exprobs_list, head, pars_tail, pars_tail_tail, pars_head;
//   DdNode * node1,**nodes_ex;
//   int r,i,cycle,res;
//   long iter1;
//   size_t lenNodes;
//   example_data * ex_d;


//   double CLL0= -2.2*pow(10,10); //-inf
//   double CLL1= -1.7*pow(10,8);  //+inf
//   double p,p0,**eta_rule;
//   double ratio,diff;


//   ex_d=(example_data *) ex_d_int;

//   init_par(ex_d,ruleInfo);


//   lenNodes=length(nodesTerm);
  
//   nodes_ex=(DdNode **)malloc(lenNodes*sizeof(DdNode*));
//   ex_d->nodes_probs=(double *)malloc(lenNodes*sizeof(double));
//   ex_d->example_prob=(double *)malloc(lenNodes*sizeof(double));

//   for (i=0;i<lenNodes;i++)
//   {
//     pair=p2p_car(nodesTerm);
//     nodesTerm=p2p_cdr(nodesTerm);
//     head=p2p_car(pair);
//     pair=p2p_cdr(pair);

//     node1=(DdNode*)p2c_int(head);
//     nodes_ex[i]=node1;
//     head=p2p_car(pair);
//     ex_d->example_prob[i]=p2c_float(head);
//   }
//   diff=CLL1-CLL0;
//   ratio=diff/fabs(CLL0);
//   if (iter==-1)
//     iter1= 2147000000;
//   else iter1=iter;

//   cycle=0;
//   while  ( (diff>ea) && (ratio>er) && (cycle<iter1) )
//   {
//     cycle++;
//     for (r=0;r<ex_d->nRules;r++)
//     {
//       if (ex_d->tunable_rules[r])
//         for (i=0;i<ex_d->rules[r]-1;i++)
//         {
//           eta_rule=ex_d->eta[r];
//           eta_rule[i][0]=0;
//           eta_rule[i][1]=0;
//         }
//     }
//     CLL0 = CLL1;
//     CLL1 = Expectation(ex_d,nodes_ex,lenNodes);
//     Maximization(ex_d);
//     diff=CLL1-CLL0;
//     ratio=diff/fabs(CLL0);
//   }
//   *pars=p2p_new();
//   c2p_list(*pars);
//   pars_list=p2p_new();
//   c2p_nil(pars_list);

//   for (r=0; r<ex_d->nRules; r++)
//   {
//     pars_rule=p2p_new();
//     c2p_nil(pars_rule);
//     p0=1;
//     for (i=0;i<ex_d->rules[r]-1;i++)
//     {
//       new_pars_rule=p2p_new();
//       c2p_list(new_pars_rule);
//       pterm=p2p_car(new_pars_rule);
//       p=ex_d->arrayprob[r][i]*p0;
//       c2p_float(p,pterm);
//       new_tail=p2p_cdr(new_pars_rule);
//       p2p_unify(new_tail,pars_rule);
//       pars_rule=new_pars_rule;
//       p0=p0*(1-ex_d->arrayprob[r][i]);
//     }
//     pars_rule_list=p2p_new();
//     c2p_list(pars_rule_list);
//     new_pars_rule=p2p_car(pars_rule_list);
//     c2p_list(new_pars_rule);
//     pterm=p2p_car(new_pars_rule);
//     c2p_float(p0,pterm);
//     new_tail=p2p_cdr(new_pars_rule);
//     p2p_unify(new_tail,pars_rule);
//     tail=p2p_cdr(pars_rule_list);
//     c2p_nil(tail);

//     new_pars_list=p2p_new();
//     c2p_list(new_pars_list);
//     compoundTerm=p2p_car(new_pars_list);
//     c2p_list(compoundTerm);
//     ruleTerm=p2p_car(compoundTerm);
//     c2p_int(r,ruleTerm);
//     tail=p2p_cdr(compoundTerm);
//     p2p_unify(pars_rule_list,tail);
//     new_tail=p2p_cdr(new_pars_list);
//     p2p_unify(pars_list,new_tail);
//     pars_list=new_pars_list;
//   }
//   pars_head=p2p_car(*pars);
//   p2p_unify(pars_list,pars_head);
//   pars_tail=p2p_cdr(*pars);
//   c2p_list(pars_tail);
//   pars_tail_tail=p2p_cdr(pars_tail);
//   c2p_nil(pars_tail_tail);


//   exprobs_list=p2p_new();
//   for (i=0;i<lenNodes;i++)
//   {
//     new_exprobs_list=p2p_new();
//     c2p_list(new_exprobs_list);
//     pterm=p2p_car(new_exprobs_list);
//     c2p_float(ex_d->nodes_probs[i],pterm);
//     tail=p2p_cdr(new_exprobs_list);
//     p2p_unify(exprobs_list,tail);
//     exprobs_list=new_exprobs_list;
//   }

//   *exprobs=p2p_new();
//   res=p2p_unify(*exprobs,exprobs_list);

//   free(nodes_ex);
//   free(ex_d->example_prob);
//   free(ex_d->nodes_probs);
//   return CLL1;
// }


/*

static int paths_to_non_zero(void)
{
  double paths;
  YAP_Term arg1,arg2,out;
  DdNode * node;

  arg1=YAP_ARG1;
  arg2=YAP_ARG2;
  node=(DdNode *)YAP_IntOfTerm(arg1);
  paths=Cudd_CountPathsToNonZero(node);
  out=YAP_MkFloatTerm(paths);
  return(YAP_Unify(out,arg2));
}

static int paths(void)
{
  double paths;
  YAP_Term arg1,arg2,out;
  DdNode * node;

  arg1=YAP_ARG1;
  arg2=YAP_ARG2;
  node=(DdNode *)YAP_IntOfTerm(arg1);
  paths=Cudd_CountPath(node);
  out=YAP_MkFloatTerm(paths);
  return(YAP_Unify(out,arg2));
}

static int dag_size(void)
{
  int size;
  YAP_Term arg1,arg2,out;
  DdNode * node;

  arg1=YAP_ARG1;
  arg2=YAP_ARG2;
  node=(DdNode *)YAP_IntOfTerm(arg1);
  size=Cudd_DagSize(node);
  out=YAP_MkIntTerm(size);
  return(YAP_Unify(out,arg2));
}
*/

FILE * open_file(char *filename, const char *mode)
/* opens a file */
{
  FILE *fp;

  if ((fp = fopen(filename, mode)) == NULL)
  {
    perror(filename);
    exit(1);
  }
  return fp;
}


tablerow* init_table(int varcnt) {
  int i;
  tablerow *tab;

  tab = (tablerow *) malloc(sizeof(rowel) * varcnt);
  for (i = 0; i < varcnt; i++)
  {
    tab[i].row = NULL;
    tab[i].cnt = 0;
  }
  return tab;
}


void add_node(tablerow *tab, DdNode *node, double value) {
  int index = Cudd_NodeReadIndex(node);

  tab[index].row = (rowel *) realloc(tab[index].row,
    (tab[index].cnt + 1) * sizeof(rowel));
  tab[index].row[tab[index].cnt].key = node;
  tab[index].row[tab[index].cnt].value = value;
  tab[index].cnt += 1;
}

void add_or_replace_node(tablerow *tab, DdNode *node, double value)
{
  int i;
  int index = Cudd_NodeReadIndex(node);
  for(i = 0; i < tab[index].cnt; i++)
  {
    if (tab[index].row[i].key == node)
    {
      tab[index].row[i].value=value;
      return;
    }
  }
  tab[index].row = (rowel *) realloc(tab[index].row,
    (tab[index].cnt + 1) * sizeof(rowel));
  tab[index].row[tab[index].cnt].key = node;
  tab[index].row[tab[index].cnt].value = value;
  tab[index].cnt += 1;
}

double * get_value(tablerow *tab,  DdNode *node) {
  int i;
  int index = Cudd_NodeReadIndex(node);

  for(i = 0; i < tab[index].cnt; i++)
  {
    if (tab[index].row[i].key == node)
    {
      return &tab[index].row[i].value;
    }
  }
  return NULL;
}

void destroy_table(tablerow *tab,int varcnt)
{
  int i;

  for (i = 0; i < varcnt; i++)
  {
    free(tab[i].row);
  }
  free(tab);
}

expltablerow* expl_init_table(int varcnt) {
  int i;
  expltablerow *tab;

  tab = (expltablerow *) malloc(sizeof(explrowel) * varcnt);
  for (i = 0; i < varcnt; i++)
  {
    tab[i].row = NULL;
    tab[i].cnt = 0;
  }
  return tab;
}


void expl_add_node(expltablerow *tab, DdNode *node, int comp, prob_abd_expl value) {
  int index = Cudd_NodeReadIndex(node);

  tab[index].row = (explrowel *) realloc(tab[index].row,
    (tab[index].cnt + 1) * sizeof(explrowel));
  tab[index].row[tab[index].cnt].key.node = node;
  tab[index].row[tab[index].cnt].key.comp = comp;
  tab[index].row[tab[index].cnt].value = value;
  tab[index].cnt += 1;
}

prob_abd_expl * expl_get_value(expltablerow *tab,  DdNode *node, int comp) {
  int i;
  int index = Cudd_NodeReadIndex(node);

  for(i = 0; i < tab[index].cnt; i++)
  {
    if (tab[index].row[i].key.node == node &&
       tab[index].row[i].key.comp == comp)
    {
      return &tab[index].row[i].value;
    }
  }
  return NULL;
}

void expl_destroy_table(expltablerow *tab,int varcnt)
{
  int i,j;

  for (i = 0; i < varcnt; i++)
  {
    for (j = 0; j< tab[i].cnt; j++)
    {
      free_list(tab[i].row[j].value.mpa);
    }
    free(tab[i].row);
  }
  free(tab);
}



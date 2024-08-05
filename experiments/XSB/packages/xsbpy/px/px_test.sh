PYTHON=$1

rm -f px_test_new.txt

$PYTHON -c 'from tpx import * ; tpx()' > px_test_new.txt

grep -v \# px_test_old.txt >> px_test_old_grepped.txt
grep -v \# px_test_new.txt >> px_test_new_grepped.txt

status=0
diff -w  px_test_new_grepped.txt px_test_old_grepped.txt  || status=1
if test "$status" = 0 ; then 
	echo "px successfuly tested"
	rm -f px_test_new.txt
else
	echo "px test failed: outputs differ!!!"
	diff -w  px_test_new_grepped.txt px_test_old_grepped.txt
fi

rm -f px_test_old_grepped.txt
rm -f px_test_new_grepped.txt


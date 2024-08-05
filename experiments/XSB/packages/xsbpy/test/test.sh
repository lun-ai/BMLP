
../../../bin/xsb -e "[xsbpy],init_xsbpy:test_xsbpy,halt. " >& /dev/null

grep '!!!' test_xsbpy_out_new

status=0
diff -w  test_xsbpy_out_new test_xsbpy_out_old || status=1
if test "$status" = 0 ; then 
	echo "xsbpy successfuly tested"
	rm -f test_xsbpy_out_new
else
	echo "xsbpy test failed: test_xsbpy_outs differ!!!"
	diff -w  test_xsbpy_out_new test_xsbpy_out_old
fi


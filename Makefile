all: 
	ocamlc nums.cma Helpers.ml -o helpers 
	ocamlc nums.cma Helpers.cmo CRT.ml -o crt
	ocamlc nums.cma Helpers.cmo Shamirs.ml -o shamirs
	ocamlc nums.cma Helpers.cmo Shamirs.cmo Shamir_Web_Encode.ml -o shamir_web_encode
	ocamlc nums.cma Helpers.cmo Shamirs.cmo Shamir_Web_Decode.ml -o shamir_web_decode
	ocamlc nums.cma Helpers.cmo CRT.cmo Mignotte_Encode.ml -o mignotte_encode
	ocamlc nums.cma Helpers.cmo CRT.cmo Mignotte_Decode.ml -o mignotte_decode
	ocamlc nums.cma Helpers.cmo CRT.cmo Asmuth_Encode.ml -o asmuth_encode
	ocamlc nums.cma Helpers.cmo CRT.cmo Asmuth_Decode.ml -o asmuth_decode
	
clean:
	rm -f helpers crt shamirs scheme shamir_web_encode shamir_web_decode mignotte_encode mignotte_decode asmuth_encode asmuth_decode *.cmi *.cmo

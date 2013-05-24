TO COMPILE SECRET SHARING
  run 'make all' from the terminal to compile all necessary files

TO RUN SECRET SHARING
  i) Each individual file can be run from the command line to test asserts
  ii) shamir_web_encode can be run from the command line with the integer
      arguments secret, the number of shares, and the critical number of shares
  iii) shamir_web_decode can be run from the command line with the points
       necessary to decode the secret, entered as integer arguments of each
       point's x and y components
  iv) mignotte_encode can be run from the command line with the secret as a 
      string, the number of shares, and the number of critical shares. It prints
      out the residues and moduli as pairs to the terminal window.  
  v) mignotte_decode can be run from the command line with each residue and 
     modulus input as separate arguments, and the base as the last argument. 
     This does not work for secrets that were too large and were split into 
     pieces. These secrets must be decoded from within the CRT.ml file, using
     decode_string_mignotte.  
  vi) asmuth_encode runs in the same way as mignotte_encode, taking the secret,
      the number of shares, and the number of critical shares. It prints out the
      residues and moduli as pairs to the terminal window.  
  vii) asmuth_decode runs similarly to mignotte_decode, taking residues and 
       moduli as individual arguments, followed by the base as the last 
       argument. Once again, this does not work for secrets that were too large 
       and were split. These secrets must be decoded by using functions within
       CRT.ml, specifically decode_string_asmuth.  
  

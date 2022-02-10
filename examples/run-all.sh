#!/bin/sh
preload_eval='(progn (require :asdf) (require :uiop) #-quicklisp (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))) (when (probe-file quicklisp-init) (load quicklisp-init))))'
dirs=$(ls -d */ | grep -v lgame | sed s:/::)
for dir in $dirs; do
  #cd $dir
  echo $dir
  if [[ $dir == 'vgrade' ]]; then
    echo '(hit esc or click into gradient to close)'
  fi
  sbcl --eval "$preload_eval" --script "$dir/$dir.lisp" &> /dev/null
  #cd ../
done

start:
  exercism download --exercise=$1 --track=common-lisp

submit:
  exercism submit $1/$1.lisp
  git add $1/**
  git commit -m "feat: $1"
  git push

test:
  cd $1
  sbcl --load $1-test.lisp --eval '($1-test:run-tests)'
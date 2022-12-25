start:
  exercism download --exercise=$1 --track=common-lisp

submit:
  exercism submit $1/$1.lisp
  git add .
  git commit -m "feat: $1"
  git push
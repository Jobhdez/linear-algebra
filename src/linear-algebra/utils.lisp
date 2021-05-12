(in-package :linear-algebra)

(defmacro my-zip (lst lst2 op)
  `(loop for i in ,lst
	 for j in ,lst2
	 collect (loop for g in i
		       for h in j
		       collect (funcall ,op g h))))

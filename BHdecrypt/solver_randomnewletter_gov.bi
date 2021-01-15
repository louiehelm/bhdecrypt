do
	state=48271*state and 2147483647
	new_letter=65+(abc_size*state shr 31)
loop until new_letter<>old_letter
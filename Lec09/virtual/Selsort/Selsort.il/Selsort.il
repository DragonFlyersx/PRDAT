.method public hidebysig static 
	void SelectionSort (
		int32[] arr // method argument number 0 (fx. ldarg.0)
	) cil managed 
{
	// Method begins at RVA 0x20cc
	// Header size: 12
	// Code size: 78 (0x4e)
	.maxstack 4 // max stack size is 4
	.locals init ( // declare local variables and slots
		[0] int32, // int i
		[1] int32, // int least
		[2] int32, // int tmp
		[3] int32, // int j
		[4] bool, // i > = arr.length
		[5] bool, // j > = arr.length
		[6] bool // arr[j] < arr[least]
	)

	IL_0000: nop // No operation

    // i = 0
	IL_0001: ldc.i4.0 // push constant 0 to the stack
	IL_0002: stloc.0 // store it in local variable i (i = 0)
	IL_0003: br.s IL_0041 // Jump to line IL_0041

	// loop start (head: IL_0041)
		IL_0005: nop // No operation

        // least = i
		IL_0006: ldloc.0 // Load variable 'i'
		IL_0007: stloc.1 // Store the variable 'i' in 'least'

        // int j = i + 1
		IL_0008: ldloc.0 // Load variable 'i'
		IL_0009: ldc.i4.1 // push constant 1
		IL_000a: add     // i + 1
		IL_000b: stloc.3 // store in j
		IL_000c: br.s IL_0022 // jump to line IL_0022

		// loop start (head: IL_0022)

            // if (arr[j] < arr[least]) least = j;
			IL_000e: ldarg.0 // Load argument 0 'arr'
			IL_000f: ldloc.3 // Load local variable j
			IL_0010: ldelem.i4 // Load array element 'arr[j]'
			IL_0011: ldarg.0 // load argument 0 'arr'
			IL_0012: ldloc.1 // Load local variable least
			IL_0013: ldelem.i4 // Load array element arr[least]
			IL_0014: clt // compare arr[j] < arr[least] (clt = compare less than)
			IL_0016: stloc.s 4 // store result in local variable
			IL_0018: ldloc.s 4 // load local variable 4
			IL_001a: brfalse.s IL_001e // jump to IL_001e if local variable is false (zero) otherwise continue

            // least = j
			IL_001c: ldloc.3 // load local variable j onto stack
			IL_001d: stloc.1 // store in least (least = j)
            
            // j++
			IL_001e: ldloc.3 // load local variable j onto stack
			IL_001f: ldc.i4.1 // Load constant integer 1 onto the evaluation stack
			IL_0020: add // add 1 to j 
			IL_0021: stloc.3 // store local variable j
            
            // j < arr.Length;
			IL_0022: ldloc.3 // Load j
			IL_0023: ldarg.0 // Load arr
			IL_0024: ldlen   // get the length of the array
			IL_0025: conv.i4 // convert length of list into int32 (int)
			IL_0026: clt     // compare j < arr.Length (1 if true, 0 if false)
			IL_0028: stloc.s 5 // store the compare result in stack
			IL_002a: ldloc.s 5 // load the compare result
			IL_002c: brtrue.s IL_000e // jump to IL_000e if true (non-zero)
		// end loop

        // int tmp = arr[i];
		IL_002e: ldarg.0 // Load argument 0 'arr'
		IL_002f: ldloc.0 // Load local varible 'i'
		IL_0030: ldelem.i4 // Load array element arr[i]
		IL_0031: stloc.2 // store local varible 2 'arr[i]'

        // arr[i] = arr[least]
		IL_0032: ldarg.0 // Load argument 0 'arr'
		IL_0033: ldloc.0 // Load local varible 'i'
		IL_0034: ldarg.0 // load argument 0 'arr'
		IL_0035: ldloc.1 // Load local varible 1 'least'
		IL_0036: ldelem.i4 // Load arr[least]
		IL_0037: stelem.i4 // Store element 'arr[least]' in arr[i]

        // arr[least] = tmp;
		IL_0038: ldarg.0 // load argument 0 'arr'
		IL_0039: ldloc.1 // load local variable 'least'
		IL_003a: ldloc.2 // load local variable 'tmp'
		IL_003b: stelem.i4 // store element arr[tmp] in arr[least]

		IL_003c: nop

        // i++
		IL_003d: ldloc.0 // Load Local varible 'i'
		IL_003e: ldc.i4.1 // push constant 1
		IL_003f: add // add to i (i+1)
		IL_0040: stloc.0 // store in local variable i. 
        
        // i < arr.Length;
		IL_0041: ldloc.0 // Load local variable 0 'i'
		IL_0042: ldarg.0 // load argument 0 'arr'
		IL_0043: ldlen // get 'arr' length
		IL_0044: conv.i4 // convert array lenght into INT32
		IL_0045: clt // compare i < arr.length
		IL_0047: stloc.s 6 // Store local variable 6 '1 or 0' (true/false)
		IL_0049: ldloc.s 6 // Load local variable 6 ''
		IL_004b: brtrue.s IL_0005 // Go to IL_0005 if 1 (true)
	// end loop

	IL_004d: ret // return
} // end of method Selsort::SelectionSort
select case ngs
	case 2 '2-grams <------------------------------------------------------------
		select case map2b(curr_symbol,k)
			case 0
				for i=0 to mc
					blt=g2(sol(j),(rl(i)*26))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 1
				for i=0 to mc
					blt=g2(rl(i),(sol(j+1)*26))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
		end select
	case 3 '3-grams <------------------------------------------------------------
		select case map2b(curr_symbol,k)
			case 0
				for i=0 to mc
					blt=g3(sol(j),sol(j+1),rl(i))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 1
				for i=0 to mc
					blt=g3(sol(j),rl(i),sol(j+2))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 2
				for i=0 to mc
					blt=g3(rl(i),sol(j+1),sol(j+2))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 3
		end select
	case 4 '4-grams <------------------------------------------------------------
		select case map2b(curr_symbol,k)
			case 0
				for i=0 to mc
					blt=g4(sol(j),sol(j+1),sol(j+2),rl(i))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 1
				for i=0 to mc
					blt=g4(sol(j),sol(j+1),rl(i),sol(j+3))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 2
				for i=0 to mc
					blt=g4(sol(j),rl(i),sol(j+2),sol(j+3))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 3
				for i=0 to mc
					blt=g4(rl(i),sol(j+1),sol(j+2),sol(j+3))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
		end select
	case 5 '5-grams <------------------------------------------------------------
		select case map2b(curr_symbol,k)
			case 0
				for i=0 to mc
					blt=g5(sol(j),sol(j+1),sol(j+2),sol(j+3),rl(i))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 1
				for i=0 to mc
					blt=g5(sol(j),sol(j+1),sol(j+2),rl(i),sol(j+4))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 2
				for i=0 to mc
					blt=g5(sol(j),sol(j+1),rl(i),sol(j+3),sol(j+4))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 3
				for i=0 to mc
					blt=g5(sol(j),rl(i),sol(j+2),sol(j+3),sol(j+4))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 4
				for i=0 to mc
					blt=g5(rl(i),sol(j+1),sol(j+2),sol(j+3),sol(j+4))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
		end select
	case 6 '6-grams <------------------------------------------------------------
		select case map2b(curr_symbol,k)
			case 0
				for i=0 to mc
					blt=g6(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),rl(i))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 1
				for i=0 to mc
					blt=g6(sol(j),sol(j+1),sol(j+2),sol(j+3),rl(i),sol(j+5))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 2
				for i=0 to mc
					blt=g6(sol(j),sol(j+1),sol(j+2),rl(i),sol(j+4),sol(j+5))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 3
				for i=0 to mc
					blt=g6(sol(j),sol(j+1),rl(i),sol(j+3),sol(j+4),sol(j+5))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 4
				for i=0 to mc
					blt=g6(sol(j),rl(i),sol(j+2),sol(j+3),sol(j+4),sol(j+5))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 5
				for i=0 to mc
					blt=g6(rl(i),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
      end select
	case 7 '7-grams <------------------------------------------------------------
		select case map2b(curr_symbol,k)
			case 0
				for i=0 to mc
					blt=g7(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5),rl(i))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 1
				for i=0 to mc
					blt=g7(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),rl(i),sol(j+6))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 2
				for i=0 to mc
					blt=g7(sol(j),sol(j+1),sol(j+2),sol(j+3),rl(i),sol(j+5),sol(j+6))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 3
				for i=0 to mc
					blt=g7(sol(j),sol(j+1),sol(j+2),rl(i),sol(j+4),sol(j+5),sol(j+6))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 4
				for i=0 to mc
					blt=g7(sol(j),sol(j+1),rl(i),sol(j+3),sol(j+4),sol(j+5),sol(j+6))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 5
				for i=0 to mc
					blt=g7(sol(j),rl(i),sol(j+2),sol(j+3),sol(j+4),sol(j+5),sol(j+6))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 6
				for i=0 to mc
					blt=g7(rl(i),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5),sol(j+6))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
      end select
	case 8 '8-grams <------------------------------------------------------------
		select case map2b(curr_symbol,k)
			case 0
				for i=0 to mc
					blt=g8(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5),sol(j+6),rl(i))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 1
				for i=0 to mc
					blt=g8(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5),rl(i),sol(j+7))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 2
				for i=0 to mc
					blt=g8(sol(j),sol(j+1),sol(j+2),sol(j+3),sol(j+4),rl(i),sol(j+6),sol(j+7))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 3
				for i=0 to mc
					blt=g8(sol(j),sol(j+1),sol(j+2),sol(j+3),rl(i),sol(j+5),sol(j+6),sol(j+7))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 4
				for i=0 to mc
					blt=g8(sol(j),sol(j+1),sol(j+2),rl(i),sol(j+4),sol(j+5),sol(j+6),sol(j+7))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 5
				for i=0 to mc
					blt=g8(sol(j),sol(j+1),rl(i),sol(j+3),sol(j+4),sol(j+5),sol(j+6),sol(j+7))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 6
				for i=0 to mc
					blt=g8(sol(j),rl(i),sol(j+2),sol(j+3),sol(j+4),sol(j+5),sol(j+6),sol(j+7))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
	      case 7
				for i=0 to mc
					blt=g8(rl(i),sol(j+1),sol(j+2),sol(j+3),sol(j+4),sol(j+5),sol(j+6),sol(j+7))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
      end select
end select
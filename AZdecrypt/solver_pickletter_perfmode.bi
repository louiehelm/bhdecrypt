select case ngs
	case 2 '2-grams <------------------------------------------------------------
		select case map2b(curr_symbol,k)
			case 0
				nc=sol(j)
				for i=0 to mc
					blt=q2g(nc+rl(i)*26)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 1
				nc=sol(j+1)*26
				for i=0 to mc
					blt=q2g(nc+rl(i))
					if blt>bls then
						bls=blt
						bl2=rl(i)
					end if
				next i
		end select
	case 3 '3-grams <------------------------------------------------------------
		select case map2b(curr_symbol,k)
			case 0
				nc=sol(j)+(sol(j+1)*26)
				for i=0 to mc
					blt=q3g(nc+rl(i)*676)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 1
				nc=sol(j)+(sol(j+2)*676)
				for i=0 to mc
					blt=q3g(nc+rl(i)*26)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 2
				nc=(sol(j+1)*26)+(sol(j+2)*676)
				for i=0 to mc
					blt=q3g(nc+rl(i))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 3
		end select
	case 4 '4-grams <------------------------------------------------------------
		select case map2b(curr_symbol,k)
			case 0
				nc=sol(j)+(sol(j+1)*26)+(sol(j+2)*676)
				for i=0 to mc
					blt=q4g(nc+rl(i)*17576)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 1
				nc=sol(j)+(sol(j+1)*26)+(sol(j+3)*17576)
				for i=0 to mc
					blt=q4g(nc+rl(i)*676)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 2
				nc=sol(j)+(sol(j+2)*676)+(sol(j+3)*17576)
				for i=0 to mc
					blt=q4g(nc+rl(i)*26)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 3
				nc=(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+3)*17576)
				for i=0 to mc
					blt=q4g(nc+rl(i))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
		end select
	case 5 '5-grams <------------------------------------------------------------
		select case map2b(curr_symbol,k)
			case 0
				'nc=ngramc(j)-sol(j+4)*456976
				nc=sol(j)+(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+3)*17576)
				for i=0 to mc
					blt=q5g(nc+rl(i)*456976)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 1
				'nc=ngramc(j)-sol(j+3)*17576
				nc=sol(j)+(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+4)*456976)
				for i=0 to mc
					blt=q5g(nc+rl(i)*17576)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 2
				'nc=ngramc(j)-sol(j+2)*676
				nc=sol(j)+(sol(j+1)*26)+(sol(j+3)*17576)+(sol(j+4)*456976)
				for i=0 to mc
					blt=q5g(nc+rl(i)*676)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 3
				'nc=ngramc(j)-sol(j+1)*26
				nc=sol(j)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+4)*456976)
				for i=0 to mc
					blt=q5g(nc+rl(i)*26)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 4
				'nc=ngramc(j)-sol(j)
	 			nc=(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+4)*456976)
				for i=0 to mc
					blt=q5g(nc+rl(i))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
		end select
	case 6 '6-grams <------------------------------------------------------------
		select case map2b(curr_symbol,k)
			case 0
				nc=sol(j)+(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+4)*456976)
				for i=0 to mc
					blt=q6g(nc+rl(i)*11881376)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 1
				nc=sol(j)+(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+5)*11881376)
				for i=0 to mc
					blt=q6g(nc+rl(i)*456976)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 2
				nc=sol(j)+(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+4)*456976)+(sol(j+5)*11881376)
				for i=0 to mc
					blt=q6g(nc+rl(i)*17576)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 3
				nc=sol(j)+(sol(j+1)*26)+(sol(j+3)*17576)+(sol(j+4)*456976)+(sol(j+5)*11881376)
				for i=0 to mc
					blt=q6g(nc+rl(i)*676)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 4
	 			nc=sol(j)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+4)*456976)+(sol(j+5)*11881376)
				for i=0 to mc
					blt=q6g(nc+rl(i)*26)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 5
	 			nc=(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+4)*456976)+(sol(j+5)*11881376)
				for i=0 to mc
					blt=q6g(nc+rl(i))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
      end select
	case 7 '7-grams <------------------------------------------------------------
		select case map2b(curr_symbol,k)
			case 0
				'nc=ngramc(j)-sol(j+6)*308915776
				nc=sol(j)+(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+4)*456976)+(sol(j+5)*11881376)
				for i=0 to mc
					blt=q7g(nc+rl(i)*308915776)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 1
				'nc=ngramc(j)-sol(j+5)*11881376
				nc=sol(j)+(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+4)*456976)+(sol(j+6)*308915776)
				for i=0 to mc
					blt=q7g(nc+rl(i)*11881376)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 2
				'nc=ngramc(j)-sol(j+4)*456976
				nc=sol(j)+(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+5)*11881376)+(sol(j+6)*308915776)
				for i=0 to mc
					blt=q7g(nc+rl(i)*456976)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 3
				'nc=ngramc(j)-sol(j+3)*17576
				nc=sol(j)+(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+4)*456976)+(sol(j+5)*11881376)+(sol(j+6)*308915776)
				for i=0 to mc
					blt=q7g(nc+rl(i)*17576)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 4
				'nc=ngramc(j)-sol(j+2)*676
	 			nc=sol(j)+(sol(j+1)*26)+(sol(j+3)*17576)+(sol(j+4)*456976)+(sol(j+5)*11881376)+(sol(j+6)*308915776)
				for i=0 to mc
					blt=q7g(nc+rl(i)*676)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 5
				'nc=ngramc(j)-sol(j+1)*26
	 			nc=sol(j)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+4)*456976)+(sol(j+5)*11881376)+(sol(j+6)*308915776)
				for i=0 to mc
					blt=q7g(nc+rl(i)*26)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 6
				'nc=ngramc(j)-sol(j)
	 			nc=(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+4)*456976)+(sol(j+5)*11881376)+(sol(j+6)*308915776)
				for i=0 to mc
					blt=q7g(nc+rl(i))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
      end select
	case 8 '8-grams <------------------------------------------------------------
		select case map2b(curr_symbol,k)
			case 0
				nc=sol(j)+(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+4)*456976)+(sol(j+5)*11881376)+(sol(j+6)*308915776)
				for i=0 to mc
					blt=q8g(nc+rl(i)*8031810176)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 1
				nc=sol(j)+(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+4)*456976)+(sol(j+5)*11881376)+(sol(j+7)*8031810176)
				for i=0 to mc
					blt=q8g(nc+rl(i)*308915776)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
	      case 2
				nc=sol(j)+(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+4)*456976)+(sol(j+6)*308915776)+(sol(j+7)*8031810176)
				for i=0 to mc
					blt=q8g(nc+rl(i)*11881376)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
			case 3
				nc=sol(j)+(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+5)*11881376)+(sol(j+6)*308915776)+(sol(j+7)*8031810176)
				for i=0 to mc
					blt=q8g(nc+rl(i)*456976)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
	      case 4
	 			nc=sol(j)+(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+4)*456976)+(sol(j+5)*11881376)+(sol(j+6)*308915776)+(sol(j+7)*8031810176)
				for i=0 to mc
					blt=q8g(nc+rl(i)*17576)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
	      case 5
	 			nc=sol(j)+(sol(j+1)*26)+(sol(j+3)*17576)+(sol(j+4)*456976)+(sol(j+5)*11881376)+(sol(j+6)*308915776)+(sol(j+7)*8031810176)
				for i=0 to mc
					blt=q8g(nc+rl(i)*676)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
	      case 6
	 			nc=sol(j)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+4)*456976)+(sol(j+5)*11881376)+(sol(j+6)*308915776)+(sol(j+7)*8031810176)
				for i=0 to mc
					blt=q8g(nc+rl(i)*26)
					if blt>bls then bls=blt:bl2=rl(i)
				next i
	      case 7
	 			nc=(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+4)*456976)+(sol(j+5)*11881376)+(sol(j+6)*308915776)+(sol(j+7)*8031810176)
				for i=0 to mc
					blt=q8g(nc+rl(i))
					if blt>bls then bls=blt:bl2=rl(i)
				next i
      end select
end select
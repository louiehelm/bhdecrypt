select case map2b(curr_symbol,k)
	case 0
		z=sol(j)+(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+4)*456976)+(sol(j+5)*11881376)+(sol(j+6)*308915776)
		if q7g(z)=1 then
			'h=sol(j)+(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+4)*456976)+(sol(j+5)*11881376)+(sol(j+6)*308915776)+(sol(j+7)*8031810176)
			h=z+(sol(j+7)*8031810176)
			for i=0 to mc
				r=h+(rl(i)*208827064576)
				pl=sa_pl9(r/dv)
				pu=sa_pu9(r/dv)
				e=1
				do
					p=(pl+pu)/2
					if r>sa_ng9(p) then pl=p else pu=p
					if r=sa_ng9(p) then ng=p:exit do
					if pu=pl then e=0:exit do
					if pu-pl=1 then
						if r=sa_ng9(pl) then ng=pl:exit do
						if r=sa_ng9(pu) then ng=pu:exit do
						e=0:exit do
					end if
				loop
				if e=1 then blt=sa_log9(ng) else blt=0
				if blt>bls then bls=blt:bl2=rl(i)
			next i
		end if
	case 1
		z=sol(j)+(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+4)*456976)+(sol(j+5)*11881376)+(sol(j+6)*308915776)
		if q7g(z)=1 then
			'h=sol(j)+(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+4)*456976)+(sol(j+5)*11881376)+(sol(j+6)*308915776)+(sol(j+8)*208827064576)
			h=z+(sol(j+8)*208827064576)
			for i=0 to mc
				r=h+(rl(i)*8031810176)
				pl=sa_pl9(r/dv)
				pu=sa_pu9(r/dv)
				e=1
				do
					p=(pl+pu)/2
					if r>sa_ng9(p) then pl=p else pu=p
					if r=sa_ng9(p) then ng=p:exit do
					if pu=pl then e=0:exit do
					if pu-pl=1 then
						if r=sa_ng9(pl) then ng=pl:exit do
						if r=sa_ng9(pu) then ng=pu:exit do
						e=0:exit do
					end if
				loop
				if e=1 then blt=sa_log9(ng) else blt=0
				if blt>bls then bls=blt:bl2=rl(i)
			next i
		end if
	case 2
		h=sol(j)+(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+4)*456976)+(sol(j+5)*11881376)+(sol(j+7)*8031810176)+(sol(j+8)*208827064576)
		for i=0 to mc
			r=h+(rl(i)*308915776)
			pl=sa_pl9(r/dv)
			pu=sa_pu9(r/dv)
			e=1
			do
				p=(pl+pu)/2
				if r>sa_ng9(p) then pl=p else pu=p
				if r=sa_ng9(p) then ng=p:exit do
				if pu=pl then e=0:exit do
				if pu-pl=1 then
					if r=sa_ng9(pl) then ng=pl:exit do
					if r=sa_ng9(pu) then ng=pu:exit do
					e=0:exit do
				end if
			loop
			if e=1 then blt=sa_log9(ng) else blt=0
			if blt>bls then bls=blt:bl2=rl(i)
		next i
	case 3
		h=sol(j)+(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+4)*456976)+(sol(j+6)*308915776)+(sol(j+7)*8031810176)+(sol(j+8)*208827064576)
		for i=0 to mc
			r=h+(rl(i)*11881376)
			pl=sa_pl9(r/dv)
			pu=sa_pu9(r/dv)
			e=1
			do
				p=(pl+pu)/2
				if r>sa_ng9(p) then pl=p else pu=p
				if r=sa_ng9(p) then ng=p:exit do
				if pu=pl then e=0:exit do
				if pu-pl=1 then
					if r=sa_ng9(pl) then ng=pl:exit do
					if r=sa_ng9(pu) then ng=pu:exit do
					e=0:exit do
				end if
			loop
			if e=1 then blt=sa_log9(ng) else blt=0
			if blt>bls then bls=blt:bl2=rl(i)
		next i
	case 4
		h=sol(j)+(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+5)*11881376)+(sol(j+6)*308915776)+(sol(j+7)*8031810176)+(sol(j+8)*208827064576)
		for i=0 to mc
			r=h+(rl(i)*456976)
			pl=sa_pl9(r/dv)
			pu=sa_pu9(r/dv)
			e=1
			do
				p=(pl+pu)/2
				if r>sa_ng9(p) then pl=p else pu=p
				if r=sa_ng9(p) then ng=p:exit do
				if pu=pl then e=0:exit do
				if pu-pl=1 then
					if r=sa_ng9(pl) then ng=pl:exit do
					if r=sa_ng9(pu) then ng=pu:exit do
					e=0:exit do
				end if
			loop
			if e=1 then blt=sa_log9(ng) else blt=0
			if blt>bls then bls=blt:bl2=rl(i)
		next i
	case 5
		h=sol(j)+(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+4)*456976)+(sol(j+5)*11881376)+(sol(j+6)*308915776)+(sol(j+7)*8031810176)+(sol(j+8)*208827064576)
		for i=0 to mc
			r=h+(rl(i)*17576)
			pl=sa_pl9(r/dv)
			pu=sa_pu9(r/dv)
			e=1
			do
				p=(pl+pu)/2
				if r>sa_ng9(p) then pl=p else pu=p
				if r=sa_ng9(p) then ng=p:exit do
				if pu=pl then e=0:exit do
				if pu-pl=1 then
					if r=sa_ng9(pl) then ng=pl:exit do
					if r=sa_ng9(pu) then ng=pu:exit do
					e=0:exit do
				end if
			loop
			if e=1 then blt=sa_log9(ng) else blt=0
			if blt>bls then bls=blt:bl2=rl(i)
		next i
	case 6
		h=sol(j)+(sol(j+1)*26)+(sol(j+3)*17576)+(sol(j+4)*456976)+(sol(j+5)*11881376)+(sol(j+6)*308915776)+(sol(j+7)*8031810176)+(sol(j+8)*208827064576)
		for i=0 to mc
			r=h+(rl(i)*676)
			pl=sa_pl9(r/dv)
			pu=sa_pu9(r/dv)
			e=1
			do
				p=(pl+pu)/2
				if r>sa_ng9(p) then pl=p else pu=p
				if r=sa_ng9(p) then ng=p:exit do
				if pu=pl then e=0:exit do
				if pu-pl=1 then
					if r=sa_ng9(pl) then ng=pl:exit do
					if r=sa_ng9(pu) then ng=pu:exit do
					e=0:exit do
				end if
			loop
			if e=1 then blt=sa_log9(ng) else blt=0
			if blt>bls then bls=blt:bl2=rl(i)
		next i
	case 7
		h=sol(j)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+4)*456976)+(sol(j+5)*11881376)+(sol(j+6)*308915776)+(sol(j+7)*8031810176)+(sol(j+8)*208827064576)
		for i=0 to mc
			r=h+(rl(i)*26)
			pl=sa_pl9(r/dv)
			pu=sa_pu9(r/dv)
			e=1
			do
				p=(pl+pu)/2
				if r>sa_ng9(p) then pl=p else pu=p
				if r=sa_ng9(p) then ng=p:exit do
				if pu=pl then e=0:exit do
				if pu-pl=1 then
					if r=sa_ng9(pl) then ng=pl:exit do
					if r=sa_ng9(pu) then ng=pu:exit do
					e=0:exit do
				end if
			loop
			if e=1 then blt=sa_log9(ng) else blt=0
			if blt>bls then bls=blt:bl2=rl(i)
		next i
	case 8
		h=(sol(j+1)*26)+(sol(j+2)*676)+(sol(j+3)*17576)+(sol(j+4)*456976)+(sol(j+5)*11881376)+(sol(j+6)*308915776)+(sol(j+7)*8031810176)+(sol(j+8)*208827064576)
		for i=0 to mc
			r=h+rl(i)
			pl=sa_pl9(r/dv)
			pu=sa_pu9(r/dv)
			e=1
			do
				p=(pl+pu)/2
				if r>sa_ng9(p) then pl=p else pu=p
				if r=sa_ng9(p) then ng=p:exit do
				if pu=pl then e=0:exit do
				if pu-pl=1 then
					if r=sa_ng9(pl) then ng=pl:exit do
					if r=sa_ng9(pu) then ng=pu:exit do
					e=0:exit do
				end if
			loop
			if e=1 then blt=sa_log9(ng) else blt=0
			if blt>bls then bls=blt:bl2=rl(i)
		next i
end select
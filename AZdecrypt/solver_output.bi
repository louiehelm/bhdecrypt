if local_outputdir=1 then
	if local_outputimp=0 then 
		if best_score>local_over then improved=1
	else
		mutexlock ml1
		if best_score>global_best_score then
			global_best_score=best_score+0.00001
			if best_score>local_over then improved=1
		end if
		mutexunlock ml1
	end if
	if improved=1 then
		if local_outputbatch=0 then
			dim as string csolstring=""
			dim as integer csn,d,filenum=tn+100
			filename=thread(tn).outputdir
			if solvesub_pccyclesformat=1 then filename+=str(thread(tn).pccycles)+"_"
			filename+=str(int(best_score))+"_"
			filename+=str(int(thread(tn).ioc*10000))+"_"
			filename+=str(int(thread(tn).multiplicity*1000))
			if thread(tn).itemnumber>0 then filename+="_"+str(thread(tn).itemnumber)
			filename+=".txt"
			if solvesub_overwriteoutput=0 andalso fileexists(filename) then
				'do not overwrite existing output
			else
				'solstring="Score: "+rdc(best_score,2)+" IOC: "+rdc(thread(tn).ioc,5)+" Multiplicity: "+rdc(s/l,5)+chr(13)+chr(10)
				solstring="Score: "+rdc(best_score,2)+" IOC: "+rdc(thread(tn).ioc,5)+" Multiplicity: "+rdc(thread(tn).multiplicity,5)+chr(13)+chr(10)
				if local_advstats=1 then
					solstring+="Repeats: "+str(thread(tn).ngrams)+" PC-cycles: "+str(thread(tn).pccycles)+stt(thread(tn).sectime)+chr(13)+chr(10)
				end if
				solstring+=chr(13)+chr(10)
				select case solver_output
					case 1:solstring+="Symbols: "+str(thread(tn).effectivesymbols)+chr(13)+chr(10)+chr(13)+chr(10)
					case 2:solstring+="Average n-gram size: "+rdc(thread(tn).tmpd1,5)+chr(13)+chr(10)+chr(13)+chr(10)
					case 3:solstring+="Polyalphabetism: "+rdc((1-thread(tn).match)*100,2)+"%"+chr(13)+chr(10)+chr(13)+chr(10)
				end select
				if thread(tn).itemname<>"" then solstring+=thread(tn).itemname+chr(13)+chr(10)+chr(13)+chr(10)
				select case solver_output
					case 2 'row bound
						j=1:k=0 'k unused ???
						for i=1 to l
							k+=1
							solstring+=chr(thread(tn).sol(i))
							if lnb(i)=1 or i=l then
								solstring+=" ("+str(int(thread(tn).graph(j)))+")"
								if i<>l then
									solstring+=chr(13)+chr(10)
									k=0:j+=1
								end if
							end if
						next i
					case else
						if solvesub_bigramheatmap=1 andalso task_active="bigram substitution + crib grid" then 'heatmap
							mutexlock csolmutex
							csol(0,0)+=1
							csn=csol(0,0)
							for i=1 to l
								'csol(i,thread(tn).sol(i)-65)+=1
								csol(i,alpharev(thread(tn).sol(i)))+=1
								e=0
								h=0
								for d=0 to ngram_alphabet_size-1
									if csol(i,d)>h then 
										h=csol(i,d)
										e=d
									end if
								next d
								'csolstring+=chr(e+65)
								csolstring+=chr(alphabet(e))
								if i<l andalso i mod thread(tn).dim_x=0 then csolstring+=chr(13)+chr(10)
							next i
							mutexunlock csolmutex
						end if
						for i=1 to l
							solstring+=chr(thread(tn).sol(i))
							if i<l andalso i mod thread(tn).dim_x=0 then solstring+=chr(13)+chr(10)
						next i
				end select
				open filename for output as #filenum
				print #filenum,solstring
				close #filenum
				if solvesub_bigramheatmap=1 andalso task_active="bigram substitution + crib grid" then
					open thread(tn).outputdir+"Most common letters "+str(csn)+".txt" for output as #filenum
					print #filenum,csolstring
					close #filenum
				end if
			end if
		else 'output to batch file
			solstring="cipher_information="
			solstring+=str(int(best_score))+"_"
			solstring+=str(int(thread(tn).ioc*10000))+"_"
			solstring+=str(int((s/l)*1000))
			solstring+=", Score: "+rdc(best_score,2)
			solstring+=" IOC: "+rdc(thread(tn).ioc,5)
			solstring+=" Multiplicity: "+rdc(thread(tn).multiplicity,5)
			if local_advstats=1 then 
				solstring+=" Repeats: "+str(thread(tn).ngrams)
				solstring+=" PC-cycles: "+str(thread(tn).pccycles)
				solstring+=stt(thread(tn).sectime)
			end if
			select case solver_output
				case 1:solstring+=" Symbols: "+str(thread(tn).effectivesymbols)
				case 2:solstring+=" Average n-gram size: "+rdc(thread(tn).tmpd1,5)
				case 3:solstring+=" Polyalphabetism: "+rdc((1-thread(tn).match)*100,2)+"%"
			end select
			if thread(tn).itemname<>"" then solstring+=", "+thread(tn).itemname
			solstring+=chr(13)+chr(10)
			select case solver_output
				case 2 'row bound
					j=1:k=0
					for i=1 to l
						k+=1
						solstring+=chr(thread(tn).sol(i))
						if lnb(i)=1 or i=l then
							solstring+=" ("+str(int(thread(tn).graph(j)))+")"
							if i<>l then
								solstring+=chr(13)+chr(10)
								k=0:j+=1
							end if
						end if
					next i
				case else
					for i=1 to l
						solstring+=chr(thread(tn).sol(i))
						if i<>l andalso i mod thread(tn).dim_x=0 then solstring+=chr(13)+chr(10)
					next i
			end select
			solstring+=chr(13)+chr(10)
			mutexlock ml1
			print #3,solstring
			mutexunlock ml1
		end if
		improved=0
	end if
end if
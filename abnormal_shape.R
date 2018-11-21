
 Density_Prof=function(data,CI,m0,m1,threshold)
 {
   for(i in 1:length(data))
   {if(data[i]<0.9)
   {break;}}
   if (i <=2)
     ch0="abnormal"
   else if(i==length(data))
     ch0="abnormal"
   else{
   out=t.test(data[i:length(data)],mu=m0,conf.level=CI)
      if(out$p.value<=threshold && (t.test(data[i:length(data)],mu=0.79)$p.value)<threshold && (t.test(data[i:length(data)],mu=0.78)$p.value)<threshold && (t.test(data[i:length(data)],mu=0.77)$p.value)<threshold &&  (t.test(data[i:length(data)],mu=0.76)$p.value)<threshold &&  (t.test(data[i:length(data)],mu=0.75)$p.value)<threshold &&  (t.test(data[i:length(data)],mu=0.81)$p.value)<threshold) 
        ch0="abnormal" 
      else ch0="the oil side has  mean equal to 0.8"
   for(j in 1:(i-1))
   {if(data[j]>1.01)
   {break;}}
      if(j<3)
        ch1="abnormal"
       else if(j==(i-1))
         {
           out=t.test(data[1:i-1],mu=m1,conf.level=CI)
            if(out$p.value<=threshold && (t.test(data[1:i-1],mu=0.99,conf.level=CI)$p.value)<threshold && (t.test(data[1:i-1],mu=0.98,conf.level=CI)$p.value)<threshold) 
             ch1="abnormal"
            else ch1="the water side has mean equal to 1 too "}
         
      else
     {
     out=t.test(data[1:j],mu=0.98, alternative="less",conf.level=CI)
     if (out$p.value<threshold || (t.test(data[j:i-1],mu=m1, alternative="greater",conf.level=CI)$p.value)<threshold)
       ch1="there is an emulsion build up "
     }}
    
   if (ch0 =="abnormal" || ch1=="abnormal")
   x="abnormal"
   else 
    x=paste(ch0,ch1 , sep=" and ")
 return(x)}

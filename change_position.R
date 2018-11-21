shift=function(variable_name,index,data)
{i=which(colnames(data)==variable_name)
n=ncol(data)
data_r=data
if (i>index )
{data[,index]=data[,i]
names(data)[index]=names(data_r)[i]
data_r[,i]=NULL
data[,index+1:ncol(data)]=NULL
data[,index+1:n]=data_r[,index:ncol(data_r)]
data=data[,1:n]}
if(i<index && index!=n)
{val_i=data[,i]
data[,i]=NULL
data$empty=0
data[,index]=val_i
names(data)[index]=names(data_r)[i]
data_r[,i]=NULL
data[,index:n]=data_r[,index-1:ncol(data_r)]
data=data[,1:n]}
if(i==index)
{print("The variable is there already")}
if(index==n)
{val_i=data[,i]
data[,i]=NULL
data$empty=0
data[,index]=val_i
names(data)[index]=names(data_r)[i]}

return(data)}



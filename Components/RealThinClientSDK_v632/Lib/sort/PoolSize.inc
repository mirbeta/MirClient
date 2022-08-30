PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end

Del_Node(node:pnode);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end

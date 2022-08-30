Remove_GetParentNode:pnode;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end

Remove_AddParentNode(node:pnode);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end

Search_Min(var i:infoType):itemType;
  var
    x:pnode;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=infoNil;
    Result:=itemMin;
    end;
  end

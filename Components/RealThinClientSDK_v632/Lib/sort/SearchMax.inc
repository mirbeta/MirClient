Search_Max(var i:infoType):itemType;
  var
    x:pnode;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=infoNil;
    Result:=itemMin;
    end;
  end

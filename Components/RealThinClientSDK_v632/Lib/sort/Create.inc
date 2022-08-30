Create(size:integer{$IFDEF RTC_GENERIC}; const _min:itemType; const _nil:infoType; const _itemComparer:IComparer<itemType>{$IFDEF RTC_BINTREE}; const _infoComparer:IComparer<infoType>{$ENDIF}{$ENDIF});
  begin
  inherited Create;
{$IFDEF RTC_GENERIC}
  itemMin:=_min;
  infoNil:=_nil;
  itemC:=_itemComparer;
  {$IFDEF RTC_BINTREE}
    infoC:=_infoComparer;
  {$ENDIF}
{$ENDIF}
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tPtrPool.Create;
  z:=new_node(itemMin,infoNil,false,nil,nil);
  z^.l:=z; z^.r:=z;
  {$IFDEF RTC_BINTREE}
    z^.l2:=z; z^.r2:=z;
  {$ENDIF}
  head:=new_node(itemMin,infoNil,false,z,z);
  {$IFDEF RTC_BINTREE}
    head^.l2:=z; head^.r2:=z;
  {$ENDIF}
  New(Parents);
  end

Create(size:integer; const _min:itemType; const _nil:infoType; const _itemComparer:IComparer<itemType>);
  begin
  Create(size,_min,_nil,_itemComparer,TComparer<infoType>.Default);
  end

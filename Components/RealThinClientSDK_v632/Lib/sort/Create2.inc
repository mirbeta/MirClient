Create(size:integer; const _min:itemType; const _nil:infoType);
  begin
  Create(size,_min,_nil,TComparer<itemType>.Default);
  end

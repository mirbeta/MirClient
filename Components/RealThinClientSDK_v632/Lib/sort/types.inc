type
  pnode=^tnode;
  tnode=record
    key:itemType;
    info:infoType;
    l,r:pnode;
    b:boolean;
    {$IFDEF RTC_BINTREE}
    l2,r2:pnode;
    b2:boolean;
    {$ENDIF}
    end;

  pnodearr=^tnodearr;
  tnodearr=array of tnode;

  pParentList=^tParentList;
  tParentList=record
    Nodes:array[0..100] of pnode;
    NodeCount:byte;
    end;

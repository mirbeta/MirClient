class(TObject)
  {$IFDEF RTC_GENERIC}
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
  {$ENDIF}
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:pnode;
    xv:itemType;
  {$IFDEF RTC_BINTREE}
    xv2:infoType;
  {$ENDIF}

    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:pnode;

    {$IFDEF RTC_GENERIC}
    // Comparers
    itemC:IComparer<itemType>;
    {$IFDEF RTC_BINTREE}
      infoC:IComparer<infoType>;
    {$ENDIF}

    // Min item and info values
    itemMin:itemType;
    infoNil:infoType;
    {$ENDIF}

    // data Pool
    myPoolSize:longint;
    myPools:array of pnodearr;
    pool:tPtrPool;
    cnt:cardinal;

    // search Tree
    head,z:pnode;
    Parents:pParentList;

    procedure del_node(node:pnode);
    function new_node(const k:itemType; const i:infoType; const bi:boolean; const ll,rr:pnode):pnode;

    procedure RemoveThis(var t:pnode);

    procedure Insert_split;
    {$IFDEF RTC_BINTREE}
    procedure Insert_split2;
    {$ENDIF}

    procedure Remove_AddParentNode(node:pnode);
    function Remove_GetParentNode:pnode;

  public
  {$IFDEF RTC_GENERIC}
    constructor Create(size:integer; const _min:itemType; const _nil:infoType); overload;
    constructor Create(size:integer; const _min:itemType; const _nil:infoType; const _itemComparer:IComparer<itemType>); overload;
    {$IFDEF RTC_BINTREE}
      constructor Create(size:integer; const _min:itemType; const _nil:infoType; const _itemComparer:IComparer<itemType>; const _infoComparer:IComparer<infoType>); overload;
    {$ENDIF}
  {$ELSE}
    constructor Create(size:integer);
  {$ENDIF}

    destructor Destroy; override;

    function Empty:boolean;

    function Count:cardinal;

    procedure PoolSize(size:integer);

    function search(const v:itemType):infoType;      // Search for exact "v"
    function search_min(var i:infoType):itemType;
    function search_max(var i:infoType):itemType;
    function search_l(const v:itemType; var i:infoType):itemType;  // Search index lower than "v"
    function search_g(const v:itemType; var i:infoType):itemType;  // Search index higher than "v"
    function search_le(const v:itemType; var i:infoType):itemType;  // Search index for lower or equel to "v"
    function search_ge(const v:itemType; var i:infoType):itemType;  // Search index for higher or equal to "v"

  {$IFDEF RTC_BINTREE}
    function isearch(const v:infoType):itemType;      // Search for info, exact "v"
    function isearch_min(var i:itemType):infoType;
    function isearch_max(var i:itemType):infoType;
    function isearch_l(const v:infoType; var i:itemType):infoType;  // Search for info lower than "v"
    function isearch_g(const v:infoType; var i:itemType):infoType;  // Search for info higher than "v"
    function isearch_le(const v:infoType; var i:itemType):infoType;  // Search for info lower or equel to "v"
    function isearch_ge(const v:infoType; var i:itemType):infoType;  // Search for info higher or equal to "v"
  {$ENDIF}

    procedure change(const v:itemType;const info:infoType);
    procedure insert(const v:itemType;const info:infoType);
    procedure remove(const v:itemType);
    procedure removeall;
  public
    property RootNode:pnode read head;
    property NilNode:pnode read z;
    end

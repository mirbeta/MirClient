unit StallSystem;

interface

uses
  Windows, Grobal2;

const
  MAX_STALL_ITEM_COUNT      = 10;

type
  TClientStall = packed record
    MakeIndex: LongWord;
    Price: Integer;
    GoldType: Byte;
  end;
  TClientStallItems = packed record
    Name: string[28];
    Items: array[0..MAX_STALL_ITEM_COUNT - 1] of TClientStall;
  end;

  TClientStallInfo = packed record
    ItemCount: Integer;
    StallName: string[28];
    Items: array[0..MAX_STALL_ITEM_COUNT - 1] of TClientItem;
  end;

  TStallInfo = packed record
    Open: Boolean;                      //
    Looks: Word;                        //Ì¯Î»Íâ¹Û
    Name: string[28];
  end;

  TStallMgr = class                     //2568
    StallType: Word;
    OnSale: Boolean;
    mBlock: TClientStallInfo;

    DoShop: Boolean;
    uSelIdx: Integer;
    uBlock: TClientStallInfo;

    CurActor: Integer;
  private
  protected
  public
    constructor Create();
  end;

  //var
    //StallMgr                  : TStallMgr;

implementation

constructor TStallMgr.Create();
begin
  StallType:= 0;
  CurActor := 0;
  OnSale := False;
  DoShop := False;
  uSelIdx := -1;
  FillChar(mBlock, SizeOf(mBlock), 0);
  FillChar(uBlock, SizeOf(uBlock), 0);

end;

end.


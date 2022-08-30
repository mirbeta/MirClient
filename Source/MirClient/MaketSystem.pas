unit MaketSystem;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Dialogs,
  Grobal2, HUtil32, EDcode;

type
  TMarketItemManager = class(TObject)
  private
    FState: Integer;                    //0 = Empty , 1 = Loading 2 = Full
    FMaxPage: Integer;
    FCurrPage: Integer;
    FLoadedpage: Integer;
    FItems: TList;                      // MaketItem
    FSelectedIndex: Integer;
    FUserMode: Integer;
    FItemType: Integer;
    bFirst: Integer;
  public
    RecvCurPage: Integer;
    RecvMaxPage: Integer;
  private
    procedure RemoveAll;
    procedure InitFirst;

    function CheckIndex(index_: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load;
    procedure ReLoad;

    procedure Add(pInfo_: PTMarketITem);
    procedure Delete(index_: Integer);
    procedure Clear;

    function GetItem(index_: Integer; var rSelected: Boolean): PTMarketITem; overload;
    function GetItem(index_: Integer): PTMarketITem; overload;
    function Select(index_: Integer): Boolean;
    function IsEmpty: Boolean;
    function count: Integer;
    function GetFirst: Integer;
    function PageCount: Integer;
    function GetUserMode: Integer;
    function GetItemType: Integer;

    procedure OnMsgReadData(Msg: TDefaultMessage; body: string);
    procedure OnMsgWriteData(Msg: TDefaultMessage; body: string);

  end;
var
  g_Market                  : TMarketItemManager;

implementation

uses
  ClMain;

constructor TMarketItemManager.Create;
begin
  InitFirst;
end;

destructor TMarketItemManager.Destroy;
begin
  RemoveAll;
  FItems.Free;
  inherited;
end;

procedure TMarketItemManager.RemoveAll;
var
  i                         : Integer;
  pinfo                     : PTMarketITem;
begin
  for i := FItems.count - 1 downto 0 do begin
    pinfo := FItems.Items[i];
    if pinfo <> nil then Dispose(pinfo);
    FItems.Delete(i);
  end;
  FItems.Clear;
  FState := MAKET_STATE_EMPTY;
end;

function TMarketItemManager.CheckIndex(index_: Integer): Boolean;
begin
  if (index_ >= 0) and (index_ < FItems.count) then
    Result := True
  else
    Result := False;
end;

procedure TMarketItemManager.InitFirst;
begin
  FItems := TList.Create;
  FSelectedIndex := -1;
  FState := MAKET_STATE_EMPTY;
  RecvCurPage := 0;
  RecvMaxPage := 0;
end;

procedure TMarketItemManager.Load;
begin
  if IsEmpty and (FState = MAKET_STATE_EMPTY) then begin
    //OnMsgReadData;
  end;
end;

procedure TMarketItemManager.ReLoad;
begin
  if not IsEmpty then RemoveAll;
  Load;
end;

procedure TMarketItemManager.Add(pInfo_: PTMarketITem);
begin
  if (FItems <> nil) and (pInfo_ <> nil) then
    FItems.Add(pInfo_);
end;

procedure TMarketItemManager.Delete(index_: Integer);
begin

end;

procedure TMarketItemManager.Clear;
begin
  RemoveAll;
  InitFirst;
end;

function TMarketItemManager.Select(index_: Integer): Boolean;
begin
  Result := False;
  if CheckIndex(index_) then begin
    FSelectedIndex := index_;
    Result := True;
  end;
end;

function TMarketItemManager.IsEmpty: Boolean;
begin
  if FItems.count > 0 then
    Result := False
  else
    Result := True;
end;

function TMarketItemManager.count: Integer;
begin
  Result := FItems.count;
end;

function TMarketItemManager.GetFirst: Integer;
begin
  Result := bFirst;
end;

function TMarketItemManager.PageCount: Integer;
begin
  if FItems.count = 0 then
    Result := 0
  else
    Result := FItems.count div MAKET_ITEMCOUNT_PER_PAGE + 1;
end;

function TMarketItemManager.GetUserMode: Integer;
begin
  Result := FUserMode;
end;

function TMarketItemManager.GetItemType: Integer;
begin
  Result := FItemType;
end;

function TMarketItemManager.GetItem(
  index_: Integer;
  var rSelected: Boolean
  ): PTMarketITem;
begin
  Result := GetItem(index_);
  if Result <> nil then begin
    if index_ = FSelectedIndex then
      rSelected := True
    else
      rSelected := False;
  end;
end;

function TMarketItemManager.GetItem(
  index_: Integer
  ): PTMarketITem;
begin
  Result := nil;
  if CheckIndex(index_) then begin
    Result := PTMarketITem(FItems.Items[index_]);

  end;
end;

procedure TMarketItemManager.OnMsgReadData(Msg: TDefaultMessage; body: string);
begin

end;

procedure TMarketItemManager.OnMsgWriteData(Msg: TDefaultMessage; body: string);
var
  nCount                    : Integer;
  i                         : Integer;
  pinfo                     : PTMarketITem;
  buffer1                   : string;
  buffer2                   : string;
begin
  case Msg.ident of
    SM_MARKET_LIST: begin
        FUserMode := Msg.Recog;
        FItemType := Msg.param;
        bFirst := Msg.tag;

        buffer1 := DecodeString(body);
        if bFirst > 0 then Clear;

        buffer1 := GetValidStr3(buffer1, buffer2, ['/']);
        nCount := Str_ToInt(buffer2, 0);

        buffer1 := GetValidStr3(buffer1, buffer2, ['/']);
        RecvCurPage := Str_ToInt(buffer2, 0);

        buffer1 := GetValidStr3(buffer1, buffer2, ['/']);
        RecvMaxPage := Str_ToInt(buffer2, 0);

        for i := 0 to nCount - 1 do begin
          buffer1 := GetValidStr3(buffer1, buffer2, ['/']);
          New(pinfo);
          DecodeBuffer(buffer2, Pointer(pinfo), SizeOf(TMarketItem));
          Add(pinfo);
        end;
      end;
  end;
end;

end.

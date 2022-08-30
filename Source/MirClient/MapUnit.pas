unit MapUnit;

interface

uses
  Windows, Classes, SysUtils, Graphics, Grobal2, HUtil32, PXL.Canvas, cliUtil, MShare;

const
  SCALE                     = 4;
  MAP_BASEPATH              = '.\Map\';

  //type
    //TTerrainTypes = (ttNormal, ttObstacle);

const
  TerrainParams             : array[Boolean] of Integer = (4, -1);

type
  TCellParams = packed record
    TerrainType: Boolean;
    TCellActor: Boolean;
  end;
  TMapData = array of array of TCellParams;

  TPathMapCell = record
    Distance: Integer;
    Direction: Integer;
  end;
  TPathMapArray = array of array of TPathMapCell;

  TWaveCell = record
    X, Y: Integer;
    Cost: Integer;
    Direction: Integer;
  end;

  TPath = array of TPoint;
  pTPath = ^TPath;

  TMapPrjInfo = record
    ident: string[16];
    ColCount: Integer;
    RowCount: Integer;
  end;

  TMapHeader = packed record
    wWidth: Word;
    wHeight: Word;
    sTitle: string[15];
    UpdateDate: TDateTime;
    Reserved: array[0..23] of AnsiChar;
  end;

  TMapInfo_Old = packed record
    wBkImg: Word;
    wMidImg: Word;
    wFrImg: Word;
    btDoorIndex: byte;
    btDoorOffset: byte;
    btAniFrame: byte;
    btAniTick: byte;
    btArea: byte;
    btLight: byte;
  end;
  pTMapInfo_Old = ^TMapInfo_Old;
  TMapInfoArr_Old = array[0..1000 * 1000 - 1] of TMapInfo_Old;
  pTMapInfoArr_Old = ^TMapInfoArr_Old;

  TMapInfo_2 = packed record
    wBkImg: Word;                       //10
    wMidImg: Word;                      //12
    wFrImg: Word;                       //14
    btDoorIndex: Byte;                  //16
    btDoorOffset: Byte;                 //17
    btAniFrame: Byte;                   //18
    btAniTick: Byte;                    //19
    btArea: Byte;                       //1A
    btLight: Byte;                      //1B
    btTiles: Byte;                      //1C
    btsmTiles: Byte;                    //1D
  end;
  pTMapInfo_2 = ^TMapInfo_2;

  TMapInfo = packed record
    wBkImg: Word;                       //0C
    wMidImg: Word;                      //0E
    wFrImg: Word;                       //10
    btDoorIndex: Byte;                  //12
    btDoorOffset: Byte;                 //13
    btAniFrame: Byte;                   //14
    btAniTick: Byte;                    //15
    btArea: Byte;                       //16
    btLight: Byte;                      //17
    btTiles: Byte;                      //18
    btsmTiles: Byte;                    //19
{.$IFDEF NEWMAP}
    wBkImg2: Word;                      //1A
    wMidImg2: Word;                     //1C
    wFrImg2: Word;                      //1E
    btDoorIndex2: Byte;                 //20
    btDoorOffset2: Byte;                //21
    wAniFrame2: Word;                   //22
    btArea2: Byte;                      //24
    btLight2: Byte;                     //25
    btTiles2: Byte;                     //26
    btsmTiles2: Byte;                   //27
    tempArr: array[0..7] of Byte;
{.$ENDIF}
  end;
  pTMapInfo = ^TMapInfo;

  TMapInfoArr = array[0..1000 * 1000 - 1] of TMapInfo;
  pTMapInfoArr = ^TMapInfoArr;

  TMapInfoArr_2 = array[0..1000 * 1000 - 1] of TMapInfo_2;
  pTMapInfoArr_2 = ^TMapInfoArr_2;

  TMArr_Old = array[0..MAXX * 3, 0..MAXY * 3] of TMapInfo_Old;
  pTMArr_Old = ^TMArr_Old;

  TMArr = array[0..MAXX * 3, 0..MAXY * 3] of TMapInfo;

  TWave = class
  private
    FData: array of TWaveCell;
    FPos: Integer;
    FCount: Integer;
    FMinCost: Integer;
    function GetItem: TWaveCell;
  public
    property item: TWaveCell read GetItem;
    property MinCost: Integer read FMinCost;
    constructor Create;
    destructor Destroy; override;
    procedure Add(NewX, NewY, NewCost, NewDirection: Integer);
    procedure Clear;
    function start: Boolean;
    function Next: Boolean;
  end;

  TPathMap = class
    m_MapHeader: TMapHeader;
    m_MapData: TMapData;
    m_MapBuf: pTMapInfoArr;
    m_nPathWidth: Integer;
    m_PathMapArray: TPathMapArray;
  public
    constructor Create;
    function FindPathOnMap(X, Y: Integer): TPath;
    function DirToDX(Direction: Integer): Integer;
    function DirToDY(Direction: Integer): Integer;
  protected
    function GetCost(X, Y, Direction: Integer): Integer; //virtual;
    function FillPathMap(X1, Y1, X2, Y2: Integer): TPathMapArray;
  end;

  TMap = class(TPathMap)
  private
    FSendRequestList: TStringlist;
    FPath: TPath;
    procedure UpdateMapSeg(cx, cy: Integer);
    procedure LoadMapArr(nCurrX, nCurrY: Integer);
    //procedure SaveMapArr(nCurrX, nCurrY: Integer);
  public
    m_MArr: TMArr;
    m_boChange: Boolean;
    m_ClientRect: TRect;
    m_OClientRect: TRect;
    m_OldClientRect: TRect;
    m_nBlockLeft: Integer;
    m_nBlockTop: Integer;
    m_nOldLeft: Integer;
    m_nOldTop: Integer;
    m_sOldMap: string;
    m_nCurUnitX: Integer;
    m_nCurUnitY: Integer;
    m_sCurrentMap: string;
    //m_sCurrentMapDes: string;
    m_nCurrentMap: Integer;
    m_nSegXCount: Integer;
    m_nSegYCount: Integer;
    m_boAllowNewMap: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure UpdateMapSquare(cx, cy: Integer);
    procedure UpdateMapPos(mx, my: Integer);
    procedure ReadyReload;
    procedure LoadMap(sMapName: string; nMx, nMy: Integer);
    procedure LoadMapData(bFirst: Boolean = False);
    function ReLoadMapData(IntActor: Boolean = True): Boolean;
    procedure MarkCanWalk(mx, my: Integer; bowalk: Boolean);
    function CanMove(mx, my: Integer): Boolean;
    function CanFly(mx, my: Integer): Boolean;
    function GetDoor(mx, my: Integer): Integer;
    function IsDoorOpen(mx, my: Integer): Boolean;
    function OpenDoor(mx, my: Integer): Boolean;
    function CloseDoor(mx, my: Integer): Boolean;
    property Path: TPath read FPath write FPath;
    function FindPath(StartX, StartY, StopX, StopY, PathSpace: Integer): TPath; overload;
    function FindPath(StopX, StopY: Integer): TPath; overload;
    procedure SetStartPos(StartX, StartY, PathSpace: Integer);
  end;
  TOnAddDownloadFile = procedure(const AFileName: String; Important: Boolean) of Object;
  function GetMapFileName(const DefaultMapDir, MapFile: String; AllowNew: Boolean): String;

var
  OnAddDownloadMapFile: TOnAddDownloadFile = nil;
  g_MapPath                 : TPath;

implementation

uses
  ClMain, Actor;

function GetMapFileName(const DefaultMapDir, MapFile: String; AllowNew: Boolean): String;
var
  AMapFile: String;
begin

  AMapFile := MapFile;
  if AMapFile[1] = '$' then
  begin
    Delete(AMapFile, 1, 1);
    Result := ResourceDir + 'Map\' + AMapFile;
    if ExtractFileExt(Result) = '' then
      Result := Result + '.map';
  end
  else
  begin
    if ExtractFileExt(AMapFile) = '' then
      AMapFile := AMapFile + '.map';
    Result := ResourceDir + 'Map\' + AMapFile;
    if not FileExists(Result) then
    begin
      if AllowNew and FileExists(DefaultMapDir + 'n' + AMapFile) then
        Result := DefaultMapDir + 'n' + AMapFile
      else
        Result := DefaultMapDir + AMapFile;
    end;
  end;
  if not FileExists(Result) then
  begin
    if Assigned(OnAddDownloadMapFile) then
      OnAddDownloadMapFile(Result, True);
  end;
end;


constructor TPathMap.Create;
begin
  inherited;
  m_MapBuf := nil;
  m_nPathWidth := 0;
end;

function TPathMap.FindPathOnMap(X, Y: Integer): TPath;
var
  Direction                 : Integer;
begin
  Result := nil;
  if (X >= m_MapHeader.wWidth) or (Y >= m_MapHeader.wHeight) then
    Exit;
  if m_PathMapArray[Y, X].Distance < 0 then
    Exit;
  SetLength(Result, m_PathMapArray[Y, X].Distance + 1);
  while m_PathMapArray[Y, X].Distance > 0 do begin
    Result[m_PathMapArray[Y, X].Distance] := Point(X, Y);
    Direction := m_PathMapArray[Y, X].Direction;
    X := X - DirToDX(Direction);
    Y := Y - DirToDY(Direction);
  end;
  Result[0] := Point(X, Y);
end;

function TPathMap.DirToDX(Direction: Integer): Integer;
begin
  case Direction of
    0, 4: Result := 0;
    1..3: Result := 1;
  else
    Result := -1;
  end;
end;

function TPathMap.DirToDY(Direction: Integer): Integer;
begin
  case Direction of
    2, 6: Result := 0;
    3..5: Result := 1;
  else
    Result := -1;
  end;
end;

function TPathMap.GetCost(X, Y, Direction: Integer): Integer;
var
  Cost                      : Integer;
begin
  Direction := (Direction and 7);
  if (X < 0) or (X >= m_MapHeader.wWidth) or (Y < 0) or (Y >= m_MapHeader.wHeight) then
    Result := -1
  else begin
    Result := TerrainParams[m_MapData[X, Y].TerrainType or m_MapData[X, Y].TCellActor];
    if (X < m_MapHeader.wWidth - m_nPathWidth) and
      (X > m_nPathWidth) and
      (Y < m_MapHeader.wHeight - m_nPathWidth) and
      (Y > m_nPathWidth) then begin
      Cost :=
        (TerrainParams[m_MapData[X - m_nPathWidth, Y].TerrainType or m_MapData[X - m_nPathWidth, Y].TCellActor]) +
        (TerrainParams[m_MapData[X + m_nPathWidth, Y].TerrainType or m_MapData[X + m_nPathWidth, Y].TCellActor]) +
        (TerrainParams[m_MapData[X, Y - m_nPathWidth].TerrainType or m_MapData[X, Y - m_nPathWidth].TCellActor]) +
        (TerrainParams[m_MapData[X, Y + m_nPathWidth].TerrainType or m_MapData[X, Y + m_nPathWidth].TCellActor]);
      if Cost < 4 * TerrainParams[{ttNormal}False] then
        Result := -1;
    end;
    if ((Direction and 1) = 1) and (Result > 0) then
      Result := Result + (Result shr 1);
  end;
end;

function TPathMap.FillPathMap(X1, Y1, X2, Y2: Integer): TPathMapArray;
var
  X, Y                      : Integer;
  OldWave, NewWave          : TWave;
  Finished                  : Boolean;
  i                         : TWaveCell;

  procedure TestNeighbours;
  var
    X, Y, c, d              : Integer;
  begin
    for d := 0 to 7 do begin
      X := OldWave.item.X + DirToDX(d);
      Y := OldWave.item.Y + DirToDY(d);
      c := GetCost(X, Y, d);
      if (c >= 0) and (Result[Y, X].Distance < 0) then
        NewWave.Add(X, Y, c, d);
    end;
  end;

  procedure ExchangeWaves;
  var
    w                       : TWave;
  begin
    w := OldWave;
    OldWave := NewWave;
    NewWave := w;
    NewWave.Clear;
  end;

begin
  Finished := ((X1 = X2) and (Y1 = Y2));
  if Finished then Exit;

  SetLength(Result, m_MapHeader.wHeight, m_MapHeader.wWidth);
  for Y := 0 to (m_MapHeader.wHeight - 1) do
    for X := 0 to (m_MapHeader.wWidth - 1) do
      Result[Y, X].Distance := -1;

  OldWave := TWave.Create;
  NewWave := TWave.Create;
  Result[Y1, X1].Distance := 0;         // 起点Distance:=0
  OldWave.Add(X1, Y1, 0, 0);            //将起点加入OldWave
  TestNeighbours;

  //Finished := ((X1 = X2) and (Y1 = Y2));
  while not Finished do begin
    ExchangeWaves;
    if not OldWave.start then
      Break;
    repeat
      i := OldWave.item;
      i.Cost := i.Cost - OldWave.MinCost; //如果大于MinCost
      if i.Cost > 0 then
        NewWave.Add(i.X, i.Y, i.Cost, i.Direction) //更新Cost= cost-MinCost
      else begin
        if Result[i.Y, i.X].Distance >= 0 then
          Continue;
        Result[i.Y, i.X].Distance := Result[i.Y - DirToDY(i.Direction), i.X - DirToDX(i.Direction)].Distance + 1;
        Result[i.Y, i.X].Direction := i.Direction;
        Finished := ((i.X = X2) and (i.Y = Y2));
        if Finished then
          Break;
        TestNeighbours;
      end;

    until not OldWave.Next;
  end;
  NewWave.Free;
  OldWave.Free;
end;

constructor TWave.Create;
begin
  Clear;
end;

destructor TWave.Destroy;
begin
  FData := nil;
  inherited Destroy;
end;

function TWave.GetItem: TWaveCell;
begin
  Result := FData[FPos];
end;

procedure TWave.Add(NewX, NewY, NewCost, NewDirection: Integer);
begin
  if FCount >= Length(FData) then
    SetLength(FData, Length(FData) + $400 {30});
  with FData[FCount] do begin
    X := NewX;
    Y := NewY;
    Cost := NewCost;
    Direction := NewDirection;
  end;
  Inc(FCount);
  if NewCost < FMinCost then
    FMinCost := NewCost;
end;

procedure TWave.Clear;
begin
  FPos := 0;
  FCount := 0;
  FMinCost := High(Integer);
end;

function TWave.start: Boolean;
begin
  FPos := 0;
  Result := (FCount > 0);
end;

function TWave.Next: Boolean;
begin
  Result := (FPos < (FCount - 1));
  if Result then Inc(FPos);
  //Inc(FPos);
  //Result := (FPos < FCount);
end;

constructor TMap.Create;
begin
  inherited Create;
  m_ClientRect := Rect(0, 0, 0, 0);
  m_boChange := False;
  m_sCurrentMap := '';
  //m_sCurrentMapDes := '';
  m_nCurrentMap := 0;
  m_nSegXCount := 0;
  m_nSegYCount := 0;
  m_nCurUnitX := -1;
  m_nCurUnitY := -1;
  m_nBlockLeft := -1;
  m_nBlockTop := -1;
  m_sOldMap := '';
  m_boAllowNewMap := False;
  //New(m_TempMArr);
  FSendRequestList := TStringlist.Create;
  FSendRequestList.CaseSensitive := False;
  FSendRequestList.Sorted := True;
end;

destructor TMap.Destroy;
begin
  //Dispose(m_TempMArr);
  FSendRequestList.Free;
  inherited Destroy;
end;

procedure TMap.UpdateMapSeg(cx, cy: Integer);
begin
  //
end;

procedure TMap.LoadMapData(bFirst: Boolean);
var
  i, X, Y, n, nMapSize      : Integer;
  TempMapInfoArr            : pTMapInfoArr_Old;
  TempMapInfoArr2           : pTMapInfoArr_2;
  canMove                   : Boolean;
begin
  if m_nCurrentMap <> 0 then begin
    if m_MapBuf = nil then begin
      nMapSize := m_MapHeader.wWidth * SizeOf(TMapInfo) * m_MapHeader.wHeight;
      m_MapBuf := AllocMem(nMapSize);
      FileSeek(m_nCurrentMap, SizeOf(TMapHeader), 0);

      case Byte(m_MapHeader.Reserved[0]) of
        6: begin
            FileRead(m_nCurrentMap, m_MapBuf^, nMapSize);
          end;
        2: begin
            n := m_MapHeader.wWidth * SizeOf(TMapInfo_2) * m_MapHeader.wHeight;
            TempMapInfoArr2 := AllocMem(n);
            FileRead(m_nCurrentMap, TempMapInfoArr2^, n);
            for X := 0 to m_MapHeader.wWidth * m_MapHeader.wHeight - 1 do begin
              Move(TempMapInfoArr2[X], m_MapBuf[X], SizeOf(TMapInfo_2));
            end;
            FreeMem(TempMapInfoArr2);
          end;
      else begin
          n := m_MapHeader.wWidth * SizeOf(TMapInfo_Old) * m_MapHeader.wHeight;
          TempMapInfoArr := AllocMem(n);
          FileRead(m_nCurrentMap, TempMapInfoArr^, n);
          for X := 0 to m_MapHeader.wWidth * m_MapHeader.wHeight - 1 do begin
            Move(TempMapInfoArr[X], m_MapBuf[X], SizeOf(TMapInfo_Old));
          end;
          FreeMem(TempMapInfoArr);
        end;
      end;
      {if byte(m_MapHeader.Reserved[0]) <> 2 then begin     //6   SizeOf(TMapInfo_IMir) = 36
        n := m_MapHeader.wWidth * SizeOf(TMapInfo_Old) * m_MapHeader.wHeight;
        TempMapInfoArr := AllocMem(n);
        FileRead(m_nCurrentMap, TempMapInfoArr^, n);
        for X := 0 to m_MapHeader.wWidth * m_MapHeader.wHeight - 1 do begin
          Move(TempMapInfoArr[X], m_MapBuf[X], SizeOf(TMapInfo_Old));
        end;
        FreeMem(TempMapInfoArr);
      end else
        FileRead(m_nCurrentMap, m_MapBuf^, nMapSize);}
    end;
    if (m_MapBuf <> nil) then begin
      if (Length(m_MapData) <= 0) then
        SetLength(m_MapData, m_MapHeader.wWidth, m_MapHeader.wHeight);
      for X := 0 to m_MapHeader.wWidth - 1 do begin
        n := X * m_MapHeader.wHeight;
        for Y := 0 to m_MapHeader.wHeight - 1 do begin
          m_MapData[X, Y].TCellActor := False;
          //canMove := (m_MapBuf[n + Y].wBkImg and $8000) = 0;
          canMove := ((m_MapBuf[n + Y].wBkImg and $8000) + (m_MapBuf[n + Y].wFrImg and $8000)) = 0;
          if canMove then
            m_MapData[X, Y].TerrainType := False
          else
            m_MapData[X, Y].TerrainType := True;
        end;
      end;
      ReLoadMapData(False);
    end;
  end;
end;

function TMap.ReLoadMapData(IntActor: Boolean): Boolean;
var
  i, X, Y, n, nMapSize      : Integer;
  nX, nY                    : Integer;
  a, Actor                  : TActor;
begin
  Result := False;
  if (g_MySelf <> nil) and (m_nCurrentMap <> 0) and (m_MapBuf <> nil) then begin

    for nX := g_MySelf.m_nCurrX - 32 to g_MySelf.m_nCurrX + 32 do
      for nY := g_MySelf.m_nCurrY - 32 to g_MySelf.m_nCurrY + 32 do
        if (nX >= 0) and (nX < m_MapHeader.wWidth) and (nY >= 0) and (nY < m_MapHeader.wHeight) then
          m_MapData[nX, nY].TCellActor := False;

    for i := 0 to g_PlayScene.m_ActorList.count - 1 do begin
      a := TActor(g_PlayScene.m_ActorList[i]);
      if a = g_MySelf then Continue;
      if (a.m_nCurrX >= g_MySelf.m_nCurrX - 32) and (a.m_nCurrX <= g_MySelf.m_nCurrX + 32) and
        (a.m_nCurrY >= g_MySelf.m_nCurrY - 32) and (a.m_nCurrY <= g_MySelf.m_nCurrY + 32) then begin
        if (a.m_boVisible) and (a.m_boHoldPlace) and (not a.m_boDeath) then
          m_MapData[a.m_nCurrX, a.m_nCurrY].TCellActor := True;
      end;
    end;

  end;
end;

procedure TMap.LoadMapArr(nCurrX, nCurrY: Integer); //优化
var
  i, j                      : Integer;
  pos                       : Integer;
  nAline                    : Integer;
  nLx                       : Integer;
  nRx                       : Integer;
  nTy                       : Integer;
  nBy                       : Integer;
  nCn                       : Integer;
begin
  if m_nCurrentMap <> 0 then begin
    FillChar(m_MArr, SizeOf(m_MArr), #0);
    nLx := (nCurrX - 1) * LOGICALMAPUNIT;
    nRx := (nCurrX + 2) * LOGICALMAPUNIT;
    nTy := (nCurrY - 1) * LOGICALMAPUNIT;
    nBy := (nCurrY + 2) * LOGICALMAPUNIT;

    if nLx < 0 then nLx := 0;
    if nTy < 0 then nTy := 0;
    if nBy >= m_MapHeader.wHeight then nBy := m_MapHeader.wHeight;

    case Byte(m_MapHeader.Reserved[0]) of
      6: begin
          nAline := SizeOf(TMapInfo) * m_MapHeader.wHeight;
          for i := nLx to nRx - 1 do begin
            if (i >= 0) and (i < m_MapHeader.wWidth) then begin
              FileSeek(m_nCurrentMap, SizeOf(TMapHeader) + (nAline * i) + (SizeOf(TMapInfo) * nTy), 0);
              FileRead(m_nCurrentMap, m_MArr[i - nLx, 0], SizeOf(TMapInfo) * (nBy - nTy));
            end;
          end;
        end;
      2: begin
          nAline := SizeOf(TMapInfo_2) * m_MapHeader.wHeight;
          for i := nLx to nRx - 1 do begin
            if (i >= 0) and (i < m_MapHeader.wWidth) then begin
              FileSeek(m_nCurrentMap, SizeOf(TMapHeader) + (nAline * i) + (SizeOf(TMapInfo_2) * nTy), 0);
              for j := 0 to nBy - nTy - 1 do
                FileRead(m_nCurrentMap, m_MArr[i - nLx, j], SizeOf(TMapInfo_2));
            end;
          end;
        end;
    else begin
        nAline := SizeOf(TMapInfo_Old) * m_MapHeader.wHeight;
        for i := nLx to nRx - 1 do begin
          if (i >= 0) and (i < m_MapHeader.wWidth) then begin
            FileSeek(m_nCurrentMap, SizeOf(TMapHeader) + (nAline * i) + (SizeOf(TMapInfo_Old) * nTy), 0);
            for j := 0 to nBy - nTy - 1 do
              FileRead(m_nCurrentMap, m_MArr[i - nLx, j], SizeOf(TMapInfo_Old));
          end;
        end;
      end;
    end;
  end;
end;

{procedure TMap.SaveMapArr(nCurrX, nCurrY: Integer);
var
  i                         : Integer;
  k                         : Integer;
  nAline                    : Integer;
  nLx                       : Integer;
  nRx                       : Integer;
  nTy                       : Integer;
  nBy                       : Integer;
begin
  if m_nCurrentMap <> 0 then begin
    FillChar(m_MArr, SizeOf(m_MArr), #0);
    nLx := (nCurrX - 1) * LOGICALMAPUNIT;
    nRx := (nCurrX + 2) * LOGICALMAPUNIT;
    nTy := (nCurrY - 1) * LOGICALMAPUNIT;
    nBy := (nCurrY + 2) * LOGICALMAPUNIT;
    if nLx < 0 then nLx := 0;
    if nTy < 0 then nTy := 0;
    if nBy >= m_MapHeader.wHeight then nBy := m_MapHeader.wHeight;
    nAline := SizeOf(TMapInfo) * m_MapHeader.wHeight;
    for i := nLx to nRx - 1 do begin
      if (i >= 0) and (i < m_MapHeader.wWidth) then begin
        FileSeek(m_nCurrentMap, SizeOf(TMapHeader) + (nAline * i) + (SizeOf(TMapInfo) * nTy), 0);
        FileRead(m_nCurrentMap, m_MArr[i - nLx, 0], SizeOf(TMapInfo) * (nBy - nTy));
      end;
    end;
  end;
end;}

procedure TMap.ReadyReload;
begin
  m_nCurUnitX := -1;
  m_nCurUnitY := -1;
end;

procedure TMap.UpdateMapSquare(cx, cy: Integer);
begin
  if (cx <> m_nCurUnitX) or (cy <> m_nCurUnitY) then begin
    LoadMapArr(cx, cy);
    m_nCurUnitX := cx;
    m_nCurUnitY := cy;
  end;
end;

procedure TMap.UpdateMapPos(mx, my: Integer);
var
  cx, cy                    : Integer;

  procedure Unmark(xx, yy: Integer);
  var
    ax, ay                  : Integer;
  begin
    if (cx = xx div LOGICALMAPUNIT) and (cy = yy div LOGICALMAPUNIT) then begin
      ax := xx - m_nBlockLeft;
      ay := yy - m_nBlockTop;
      m_MArr[ax, ay].wFrImg := m_MArr[ax, ay].wFrImg and $7FFF;
      m_MArr[ax, ay].wBkImg := m_MArr[ax, ay].wBkImg and $7FFF;
    end;
  end;

begin
  cx := mx div LOGICALMAPUNIT;
  cy := my div LOGICALMAPUNIT;
  m_nBlockLeft := _MAX(0, (cx - 1) * LOGICALMAPUNIT);
  m_nBlockTop := _MAX(0, (cy - 1) * LOGICALMAPUNIT);

  UpdateMapSquare(cx, cy);

  if (m_nOldLeft <> m_nBlockLeft) or (m_nOldTop <> m_nBlockTop) or (m_sOldMap <> m_sCurrentMap) then begin
    if m_sCurrentMap = '3' then begin
      Unmark(624, 278);
      Unmark(627, 278);
      Unmark(634, 271);
      Unmark(564, 287);
      Unmark(564, 286);
      Unmark(661, 277);
      Unmark(578, 296);
    end;
  end;
  m_nOldLeft := m_nBlockLeft;
  m_nOldTop := m_nBlockTop;
end;

procedure TMap.LoadMap(sMapName: string; nMx, nMy: Integer);
var
  sFileName                 : string;
begin
  FillChar(m_MArr, SizeOf(m_MArr), #0);
  m_nCurUnitX := -1;
  m_nCurUnitY := -1;
//
  m_sCurrentMap := sMapName;
  if m_nCurrentMap <> 0 then begin
    FileClose(m_nCurrentMap);
    m_nCurrentMap := 0;
  end;
  sFileName := GetMapFileName(MAP_BASEPATH, m_sCurrentMap, m_boAllowNewMap);
//  sFileName := Format('%s%s%s', [MAP_BASEPATH, m_sCurrentMap, '.map']);
  //opt -> Save MapFile Handle
  if FileExists(sFileName) then begin
    m_nCurrentMap := FileOpen(sFileName, fmOpenRead or fmShareDenyNone);
    if m_nCurrentMap <> 0 then begin
      if FileRead(m_nCurrentMap, m_MapHeader, SizeOf(TMapHeader)) <> SizeOf(TMapHeader) then begin
        FileClose(m_nCurrentMap);
        m_nCurrentMap := 0;
      end;
    end;
    UpdateMapPos(nMx, nMy);
  end else begin
   //修复地图错乱
    FillChar(m_MArr, SizeOf(m_MArr), #0);
    if FSendRequestList.IndexOf(sFileName) < 0 then begin
      FSendRequestList.Add(sFileName);
//      PatchUnit.g_PatchClientManager.SendProcMsg(Self, sFileName, PM_MAP, 0);
    end;
  end;
  m_sOldMap := m_sCurrentMap;
end;

procedure TMap.MarkCanWalk(mx, my: Integer; bowalk: Boolean);
var
  cx, cy                    : Integer;
begin
  cx := mx - m_nBlockLeft;
  cy := my - m_nBlockTop;
  if (cx < 0) or (cy < 0) then Exit;
  if bowalk then
    Map.m_MArr[cx, cy].wFrImg := Map.m_MArr[cx, cy].wFrImg and $7FFF
  else
    Map.m_MArr[cx, cy].wFrImg := Map.m_MArr[cx, cy].wFrImg or $8000;
end;

function TMap.CanMove(mx, my: Integer): Boolean;
var
  cx, cy                    : Integer;
begin
  Result := False;
  cx := mx - m_nBlockLeft;
  cy := my - m_nBlockTop;
  if (cx < 0) or (cy < 0) then Exit;
  Result := ((Map.m_MArr[cx, cy].wBkImg and $8000) + (Map.m_MArr[cx, cy].wFrImg and $8000)) = 0;
  if Result then begin
    if Map.m_MArr[cx, cy].btDoorIndex and $80 > 0 then begin
      if (Map.m_MArr[cx, cy].btDoorOffset and $80) = 0 then
        Result := False;
    end;
  end;
end;

function TMap.CanFly(mx, my: Integer): Boolean;
var
  cx, cy                    : Integer;
begin
  Result := False;
  cx := mx - m_nBlockLeft;
  cy := my - m_nBlockTop;
  if (cx < 0) or (cy < 0) then Exit;
  Result := (Map.m_MArr[cx, cy].wFrImg and $8000) = 0;
  if Result then begin
    if Map.m_MArr[cx, cy].btDoorIndex and $80 > 0 then begin
      if (Map.m_MArr[cx, cy].btDoorOffset and $80) = 0 then
        Result := False;
    end;
  end;
end;

function TMap.GetDoor(mx, my: Integer): Integer;
var
  cx, cy                    : Integer;
begin
  Result := 0;
  cx := mx - m_nBlockLeft;
  cy := my - m_nBlockTop;
  if Map.m_MArr[cx, cy].btDoorIndex and $80 > 0 then begin
    Result := Map.m_MArr[cx, cy].btDoorIndex and $7F;
  end;
end;

function TMap.IsDoorOpen(mx, my: Integer): Boolean;
var
  cx, cy                    : Integer;
begin
  Result := False;
  cx := mx - m_nBlockLeft;
  cy := my - m_nBlockTop;
  if Map.m_MArr[cx, cy].btDoorIndex and $80 > 0 then begin
    Result := (Map.m_MArr[cx, cy].btDoorOffset and $80 <> 0);
  end;
end;

function TMap.OpenDoor(mx, my: Integer): Boolean;
var
  i, j, cx, cy, idx         : Integer;
begin
  Result := False;
  cx := mx - m_nBlockLeft;
  cy := my - m_nBlockTop;
  if (cx < 0) or (cy < 0) then Exit;
  if Map.m_MArr[cx, cy].btDoorIndex and $80 > 0 then begin
    idx := Map.m_MArr[cx, cy].btDoorIndex and $7F;
    for i := cx - 10 to cx + 10 do
      for j := cy - 10 to cy + 10 do begin
        if (i > 0) and (j > 0) then
          if (Map.m_MArr[i, j].btDoorIndex and $7F) = idx then
            Map.m_MArr[i, j].btDoorOffset := Map.m_MArr[i, j].btDoorOffset or $80;
      end;
  end;
end;

function TMap.CloseDoor(mx, my: Integer): Boolean;
var
  i, j, cx, cy, idx         : Integer;
begin
  Result := False;
  cx := mx - m_nBlockLeft;
  cy := my - m_nBlockTop;
  if (cx < 0) or (cy < 0) then Exit;
  if Map.m_MArr[cx, cy].btDoorIndex and $80 > 0 then begin
    idx := Map.m_MArr[cx, cy].btDoorIndex and $7F;
    for i := cx - 8 to cx + 10 do
      for j := cy - 8 to cy + 10 do begin
        if (Map.m_MArr[i, j].btDoorIndex and $7F) = idx then
          Map.m_MArr[i, j].btDoorOffset := Map.m_MArr[i, j].btDoorOffset and $7F;
      end;
  end;
end;

function TMap.FindPath(StartX, StartY, StopX, StopY, PathSpace: Integer): TPath;
begin
  m_nPathWidth := PathSpace;
  m_PathMapArray := FillPathMap(StartX, StartY, StopX, StopY); //费时
  Result := FindPathOnMap(StopX, StopY);
end;

function TMap.FindPath(StopX, StopY: Integer): TPath;
begin
  Result := FindPathOnMap(StopX, StopY);
end;

procedure TMap.SetStartPos(StartX, StartY, PathSpace: Integer);
begin
  m_nPathWidth := PathSpace;
  m_PathMapArray := FillPathMap(StartX, StartY, -1, -1);
end;

end.


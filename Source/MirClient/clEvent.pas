unit clEvent;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PXL.Canvas, PXL.Textures, uGameEngine, Grobal2, ExtCtrls, HUtil32, EDcode,
  cliUtil, MShare;

const
{$IF CUSTOMLIBFILE = 1}
  ZOMBIDIGUPDUSTBASE        = 420;
  STONEFRAGMENTBASE         = 10;
  HOLYCURTAINBASE           = 20;
  FIREBURNBASE              = 40;
  SCULPTUREFRAGMENT         = 59;
{$ELSE}
  ZOMBIDIGUPDUSTBASE        = 420;
  STONEFRAGMENTBASE         = 64;
  HOLYCURTAINBASE           = 1390;
  FIREBURNBASE              = 1630;
  SCULPTUREFRAGMENT         = 1349;
{$IFEND}
type
  TClEvent = class
    m_nX: Integer;
    m_nY: Integer;
    m_nDir: Integer;
    m_nPx: Integer;
    m_nPy: Integer;
    m_nEventType: Integer;
    m_nEventParam: Integer;
    m_nEventLevel: Integer;
    m_nServerId: Integer;
    m_Dsurface: TCustomLockableTexture;
    m_boBlend: Boolean;
    m_dwFrameTime: LongWord;
    m_dwCurframe: LongWord;
    m_nLight: Integer;
  private
  public
    constructor Create(svid, ax, ay, evtype: Integer);
    destructor Destroy; override;
    procedure DrawEvent(backsurface: TCustomCanvas; ax, ay: Integer); virtual;
    procedure Run; virtual;
  end;

  TClEventManager = class
  private
  public
    EventList: TList;
    constructor Create;
    destructor Destroy; override;
    procedure ClearEvents;
    function AddEvent(evn: TClEvent): TClEvent;
    procedure DelEvent(evn: TClEvent);
    procedure DelEventById(svid: Integer);
    function GetEvent(ax, ay, etype: Integer): TClEvent;
    procedure Execute;
  end;

implementation

uses
  ClMain;

constructor TClEvent.Create(svid, ax, ay, evtype: Integer);
begin
  m_nServerId := svid;
  m_nX := ax;
  m_nY := ay;
  m_nEventType := evtype;
  m_nEventParam := 0;
  m_boBlend := False;
  m_dwFrameTime := GetTickCount;
  m_dwCurframe := 0;
  m_nLight := 0;
  m_nEventLevel := 0;
end;

destructor TClEvent.Destroy;
begin
  inherited Destroy;
end;

procedure TClEvent.DrawEvent(backsurface: TCustomCanvas; ax, ay: Integer);
begin
  if m_Dsurface <> nil then
    if m_boBlend then
      backsurface.DrawBlend(ax + m_nPx, ay + m_nPy, m_Dsurface, 1)
    else
      backsurface.Draw(ax + m_nPx, ay + m_nPy, m_Dsurface.ClientRect, m_Dsurface, True);
end;

procedure TClEvent.Run;
begin
  m_Dsurface := nil;
  if GetTickCount - m_dwFrameTime > 20 then begin
    m_dwFrameTime := GetTickCount;
    Inc(m_dwCurframe);
  end;
  case m_nEventType of
{$IF CUSTOMLIBFILE = 1}
    ET_DIGOUTZOMBI: m_Dsurface := g_WEventEffectImages.GetCachedImage(m_nDir, m_nPx, m_nPy);
{$ELSE}
    ET_DIGOUTZOMBI: m_Dsurface := g_WMon6Img.GetCachedImage(ZOMBIDIGUPDUSTBASE + m_nDir, m_nPx, m_nPy);
{$IFEND}
    ET_PILESTONES: begin
        if m_nEventParam <= 0 then m_nEventParam := 1;
        if m_nEventParam > 5 then m_nEventParam := 5;
{$IF CUSTOMLIBFILE = 1}
        m_Dsurface := g_WEventEffectImages.GetCachedImage(STONEFRAGMENTBASE + (m_nEventParam - 1), m_nPx, m_nPy);
{$ELSE}
        m_Dsurface := g_WEffectImg.GetCachedImage(STONEFRAGMENTBASE + (m_nEventParam - 1), m_nPx, m_nPy);
{$IFEND}
      end;
    ET_HOLYCURTAIN: begin
{$IF CUSTOMLIBFILE = 1}
        m_Dsurface := g_WEventEffectImages.GetCachedImage(HOLYCURTAINBASE + (m_dwCurframe mod 10), m_nPx, m_nPy);
{$ELSE}
        m_Dsurface := g_WMagicImages.GetCachedImage(HOLYCURTAINBASE + (m_dwCurframe mod 10), m_nPx, m_nPy);
{$IFEND}

        m_boBlend := True;
        m_nLight := 1;
      end;
    ET_FIRE: begin
        case m_nEventLevel of
          1: m_Dsurface := g_WMagic7Images.GetCachedImage(90 + ((m_dwCurframe div 2) mod 8), m_nPx, m_nPy);
          2: m_Dsurface := g_WMagic7Images.GetCachedImage(100 + ((m_dwCurframe div 2) mod 8), m_nPx, m_nPy);
          3: m_Dsurface := g_WMagic7Images.GetCachedImage(110 + ((m_dwCurframe div 2) mod 8), m_nPx, m_nPy);
        else
          m_Dsurface := g_WMagicImages.GetCachedImage(FIREBURNBASE + ((m_dwCurframe div 2) mod 6), m_nPx, m_nPy);
        end;
        m_boBlend := True;
        m_nLight := 1;
      end;
    ET_SCULPEICE: begin
{$IF CUSTOMLIBFILE = 1}
        m_Dsurface := g_WEventEffectImages.GetCachedImage(SCULPTUREFRAGMENT, m_nPx, m_nPy);
{$ELSE}
        m_Dsurface := g_WMon7Img.GetCachedImage(SCULPTUREFRAGMENT, m_nPx, m_nPy);
{$IFEND}
      end;
    ET_NIMBUS_1: begin
        m_Dsurface := g_StateEffect.GetCachedImage(0 + (m_dwCurframe mod 19), m_nPx, m_nPy);
        m_boBlend := True;
        m_nLight := 1;
      end;
    ET_NIMBUS_2: begin
        m_Dsurface := g_StateEffect.GetCachedImage(0 + (m_dwCurframe mod 19), m_nPx, m_nPy);
        m_boBlend := True;
        m_nLight := 1;
      end;
    ET_NIMBUS_3: begin
        m_Dsurface := g_StateEffect.GetCachedImage(0 + (m_dwCurframe mod 19), m_nPx, m_nPy);
        m_boBlend := True;
        m_nLight := 1;
      end;
  end;
end;

constructor TClEventManager.Create;
begin
  EventList := TList.Create;
end;

destructor TClEventManager.Destroy;
var
  i                         : Integer;
begin
  for i := 0 to EventList.count - 1 do
    TClEvent(EventList[i]).Free;
  EventList.Free;
  inherited Destroy;
end;

procedure TClEventManager.ClearEvents;
var
  i                         : Integer;
begin
  for i := 0 to EventList.count - 1 do
    TClEvent(EventList[i]).Free;
  EventList.Clear;
end;

function TClEventManager.AddEvent(evn: TClEvent): TClEvent;
var
  i                         : Integer;
  event                     : TClEvent;
begin
  for i := 0 to EventList.count - 1 do
    if (EventList[i] = evn) or (TClEvent(EventList[i]).m_nServerId = evn.m_nServerId) then begin
      evn.Free;
      Result := nil;
      Exit;
    end;
  EventList.Add(evn);
  Result := evn;
end;

procedure TClEventManager.DelEvent(evn: TClEvent);
var
  i                         : Integer;
begin
  for i := 0 to EventList.count - 1 do
    if EventList[i] = evn then begin
      TClEvent(EventList[i]).Free;
      EventList.Delete(i);
      Break;
    end;
end;

procedure TClEventManager.DelEventById(svid: Integer);
var
  i                         : Integer;
begin
  for i := 0 to EventList.count - 1 do
    if TClEvent(EventList[i]).m_nServerId = svid then begin
      TClEvent(EventList[i]).Free;
      EventList.Delete(i);
      Break;
    end;
end;

function TClEventManager.GetEvent(ax, ay, etype: Integer): TClEvent;
var
  i                         : Integer;
begin
  Result := nil;
  for i := 0 to EventList.count - 1 do
    if (TClEvent(EventList[i]).m_nX = ax) and (TClEvent(EventList[i]).m_nY = ay) and
      (TClEvent(EventList[i]).m_nEventType = etype) then begin
      Result := TClEvent(EventList[i]);
      Break;
    end;
end;

procedure TClEventManager.Execute;
var
  i                         : Integer;
  event                     : TClEvent;
begin
  for i := 0 to EventList.count - 1 do begin
    event := TClEvent(EventList[i]);
    event.Run;
  end;
end;

end.


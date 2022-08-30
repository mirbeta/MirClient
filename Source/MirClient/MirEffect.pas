unit MirEffect;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, uGameEngine,
  Grobal2, PXL.Canvas, PXL.Textures, cliUtil, ClFunc, HUtil32, DWinCtl, WIL;

type
  TRareBoxWindow = class
    m_boActive: Boolean;
    m_boFlashStart: Boolean;
    m_boKeyAvail: Boolean;
    m_nFlashBoxTime: Integer;
    m_nFlashBoxCount: Integer;
    m_dwFlashBoxTick: LongWord;
    m_boSelBoxFlash: Boolean;
    m_boRareBoxShow: Boolean;
    m_ImgLib: TWMImages;
    m_btItemIdx: byte;
    m_btSvrItemIdx: byte;
    m_boLightFlash: Boolean;
    m_dwLightFlash: LongWord;
    m_dwFlashIndex: LongWord;
    m_btRareBtnFlash: byte;
    m_dwRareBtnFlash: LongWord;
    m_btFlashIdx: byte;
    m_nLeft: Integer;
    m_nTop: Integer;
    m_nStepTime: Integer;
    m_nEffectBase: Integer;
    m_nStart: Integer;
    m_nCurFrame: Integer;
    m_nFrame: Integer;
    m_nNextFrameTime: LongWord;
    m_BoxItems: TBoxItems;
    m_ADButton: array[1..9] of TDButton;
    m_Effect: TCustomLockableTexture;
  public
    constructor Create(xx, yy: Integer);
    procedure Initialize();
    function Run: Boolean; virtual;
    function SetActive(nShape: Integer): Boolean; virtual;
    procedure DrawEff(Surface: TCustomCanvas); virtual;
  end;

implementation

uses
  SoundUtil, FState, MShare;

{TRareBoxWindow}

constructor TRareBoxWindow.Create(xx, yy: Integer);
begin
  m_nLeft := xx;
  m_nTop := yy;
  Initialize();
end;

procedure TRareBoxWindow.Initialize();
begin
  m_ImgLib := nil;
  m_boFlashStart := False;
  m_boSelBoxFlash := False;
  m_boLightFlash := False;
  m_boKeyAvail := False;
  m_dwLightFlash := GetTickCount;
  m_dwFlashIndex := GetTickCount;
  m_btRareBtnFlash := 0;
  m_dwRareBtnFlash := GetTickCount;
  m_btItemIdx := 9;
  m_nFlashBoxTime := 5;
  m_dwFlashBoxTick := GetTickCount;
  m_nFlashBoxCount := 0;
  m_btSvrItemIdx := 0;
  m_btFlashIdx := 0;
  m_nStart := 0;
  m_nCurFrame := 0;
  m_nFrame := 6;
  m_nNextFrameTime := 200;
  m_boActive := False;
  m_boRareBoxShow := False;
  {m_ADButton[1] := frmDlg.DBRareItem1;
  m_ADButton[2] := frmDlg.DBRareItem2;
  m_ADButton[3] := frmDlg.DBRareItem3;
  m_ADButton[4] := frmDlg.DBRareItem4;
  m_ADButton[5] := frmDlg.DBRareItem5;
  m_ADButton[6] := frmDlg.DBRareItem6;
  m_ADButton[7] := frmDlg.DBRareItem7;
  m_ADButton[8] := frmDlg.DBRareItem8;
  m_ADButton[9] := frmDlg.DBRareItem9;}
end;

function TRareBoxWindow.SetActive(nShape: Integer): Boolean;
begin
  m_ImgLib := nil;
  m_nStart := 0;
  m_nCurFrame := 0;
  m_boRareBoxShow := False;
  case nShape of
    2..5: begin
        m_ImgLib := g_WMain3Images;
        m_nEffectBase := 521 + (nShape - 2) * 20;
      end;
    6: begin
        m_ImgLib := g_WMain2Images;
        m_nEffectBase := 131;
      end;
  end;
  m_boActive := m_ImgLib <> nil;
  if m_boActive then g_SndMgr.PlaySound('wav\OpenBox.wav');
  Result := m_boActive;
end;

procedure TRareBoxWindow.DrawEff(Surface: TCustomCanvas);
var
  d                         : TCustomLockableTexture;
  nRx, nRy, nPx, nPy        : Integer;
begin
  if not m_boActive or (m_ImgLib = nil) then Exit;
  if not m_boRareBoxShow then begin
    d := m_ImgLib.Images[m_nEffectBase + m_nCurFrame];
    if d <> nil then
      Surface.Draw(m_nLeft, m_nTop, d.ClientRect, d, True);
    if m_nCurFrame > 1 then begin
      d := m_ImgLib.Images[m_nEffectBase + m_nCurFrame + 7];
      if d <> nil then
        Surface.DrawBlend(m_nLeft, m_nTop, d, 1);
    end;
  end else begin

  end;
end;

function TRareBoxWindow.Run: Boolean;
begin
  Result := True;
  if m_boActive and not m_boRareBoxShow and (GetTickCount - m_nStepTime > LongWord(m_nNextFrameTime)) then begin
    m_nStepTime := GetTickCount;
    Inc(m_nCurFrame);
    if m_nCurFrame > m_nStart + m_nFrame - 1 then begin
      m_nCurFrame := m_nStart;
      m_boRareBoxShow := True;
      //Result := False;
      Exit;
    end;
  end;
  {if m_boRareBoxShow then begin
    //m_boActive := False;
  end;}
end;

end.

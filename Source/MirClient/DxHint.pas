unit DxHint;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StrUtils, Grobal2, uGameEngine, PXL.Canvas, PXL.Textures, PXL.Types,
  AsphyreTextureFonts, IntroScn,
  HumanActor, Actor, cliUtil, ClFunc, HUtil32, GList, DWinCtl,
  uCommon, Generics.Collections;

type
  TDxHintMgr = class
    m_AutoClear: Boolean;
  public
    m_HintList: TDictionary<string, pTHintStrings>;
    constructor Create;
    destructor Destroy; override;
    function ShowHint(X, Y: Integer; Str: string; Color: TColor;
      drawUp, drawLeft, ShowItem, ItemWearing: Boolean): Integer;
    procedure DrawHint(MSurface: TCustomCanvas);
    procedure ClearHint;
  end;

var
  g_DxHintMgr: TDxHintMgr;

implementation

uses
  ClMain, FState, MShare;

constructor TDxHintMgr.Create;
begin
  m_AutoClear := True;
  m_HintList := TDictionary<string, pTHintStrings>.Create;
end;

destructor TDxHintMgr.Destroy;
var
  i, ii: Integer;
  pStrins: pTHintStrings;
  List: TList;
  s: string;
begin
  for s in m_HintList.Keys do
  begin
    pStrins := m_HintList[s];
    for ii := 0 to pStrins.Strs.Count - 1 do
      Dispose(pTHintTextSegment(pStrins.Strs[ii]));
    pStrins.Strs.Free;
    Dispose(pStrins);
  end;
  m_HintList.Free;
  inherited;
end;

procedure TDxHintMgr.ClearHint;
var
  i, ii: Integer;
  pStrins: pTHintStrings;
  List: TList;
  s: string;
begin
  for s in m_HintList.Keys do
  begin
    pStrins := m_HintList[s];
    if pStrins.Show then
      pStrins.Show := False;
  end;
end;

function TDxHintMgr.ShowHint(X, Y: Integer; Str: string; Color: TColor;
  drawUp, drawLeft, ShowItem, ItemWearing: Boolean): Integer;
var
  dwCrc: Cardinal;
  data, fdata, sData, sCustom, sSet, scfg: String;
  cl: TColor;
  pStrins: pTHintStrings;
  i, j, k, w, idx, sX, nBold, ShowType: Integer;
  Segment: THintTextSegment;
  pSegment: pTHintTextSegment;
  p: Pointer;
begin
  Result := 0;
  if Str = '' then
    Exit;

  if m_AutoClear then
    ClearHint;

  ShowType := 0;
  if Color = clBlack then
    ShowType := 1;
  var
  str22 := format('%d%s', [ShowType, Str]);
  if m_HintList.Count <> 0 then
  begin
    m_HintList.TryGetValue(str22, pStrins);
    if pStrins <> nil then
    begin
      pStrins.Show := True;
      pStrins.Quad := ShowItem;
      pStrins.Brush := ItemWearing;
      pStrins.X := X;
      pStrins.Y := Y;
      pStrins.LastTick := GetTickCount;
      if drawUp then
        pStrins.Y := pStrins.Y - pStrins.Height;
      if drawLeft then
        pStrins.X := pStrins.X - pStrins.Width;
      Result := pStrins.Height;
      Exit;
    end;
  end;

  New(pStrins);
  pStrins.Show := True;
  pStrins.Crc := dwCrc;
  pStrins.X := X;
  pStrins.Y := Y;
  pStrins.Strs := TList.Create;
  pStrins.Width := 0;
  pStrins.Height := 0;
  pStrins.Quad := ShowItem;
  pStrins.Brush := ItemWearing;
  pStrins.LastTick := GetTickCount;
  pStrins.ShowType := 0;
  if Color = clBlack then
    pStrins.ShowType := 1;

  var
  str1 := format('%d%s', [ShowType, Str]);
  m_HintList.Add(str1, pStrins);

  idx := 0;
  nBold := 0;
  while True do
  begin
    if Str = '' then
      Break;
    Str := GetValidStr3(Str, data, ['\']);
    w := 0;
    if data <> '' then
    begin
      sX := 0;
      if data[1] = '-' then
      begin
        New(pSegment);
        pSegment.Text := data;
        pSegment.offsetX := sX;
        pSegment.indexY := idx;
        pSegment.Image := 0;
        pSegment.Size := 9;
        pSegment.Color := Color;
        pSegment.Bold := 0;
        pSegment.ShowLine := True;
        pStrins.Strs.Add(pSegment);
        w := w + 6 + 8;
      end
      else
      begin
        while (data <> '') and (Pos('<', data) > 0) and (Pos('>', data) > 0) do
        begin
          fdata := '';
          if data[1] <> '<' then
            data := '<' + GetValidStr3(data, fdata, ['<']);

          if fdata <> '' then
          begin
            New(pSegment);
            pSegment.Text := fdata;
            pSegment.offsetX := sX;
            pSegment.indexY := idx;
            pSegment.Image := 0;
            pSegment.Size := 9;
            pSegment.Color := Color;
            pSegment.Bold := 0;
            pSegment.ShowLine := False;
            pStrins.Strs.Add(pSegment);
            sX := sX + Length(AnsiString(fdata)) * 6;
            w := w + Length(AnsiString(fdata)) * 6;
          end;

          data := ArrestStringEx(data, '<', '>', sCustom);

          sData := '';
          Segment.Color := Color;
          Segment.Size := 9;
          Segment.Bold := 0;
          // Segment.Image := 0;
          Segment.ShowLine := False;
          if sCustom <> '' then
          begin
            sSet := GetValidStr3(sCustom, sData, ['|']);
            while sSet <> '' do
            begin
              sSet := GetValidStr3(sSet, scfg, [' ']);
              if scfg <> '' then
              begin
                scfg := UpperCase(scfg);
                case scfg[1] of
                  'I':
                    begin
                      // Image
                      scfg := Copy(scfg, 3, Length(scfg) - 2);
                      Segment.Image := StrToInt(scfg);
                    end;
                  'C':
                    begin
                      scfg := Copy(scfg, 3, Length(scfg) - 2);
                      if CompareText('clLtGray', scfg) = 0 then
                        Segment.Color := clLtGray
                      else if CompareText('clDkGray', scfg) = 0 then
                        Segment.Color := clDkGray
                      else
                        Segment.Color := StringToColor(scfg);
                    end;
                  'S':
                    begin
                      scfg := Copy(scfg, 3, Length(scfg) - 2);
                      Segment.Size := StrToInt(scfg);
                    end;
                  'B':
                    begin
                      scfg := Copy(scfg, 3, Length(scfg) - 2);
                      Segment.Bold := 0;
                      if CompareText(scfg, 'BOLD') = 0 then
                        Segment.Bold := 1
                      else if CompareText(scfg, 'UNDERLINE') = 0 then
                        Segment.Bold := 2;
                    end;
                end;
              end;
            end;
          end;
          if sData <> '' then
          begin
            New(pSegment);
            pSegment.Text := sData;
            pSegment.offsetX := sX;
            pSegment.indexY := idx;
            pSegment.Image := Segment.Image;
            pSegment.Size := Segment.Size;
            pSegment.Color := Segment.Color;
            pSegment.Bold := Segment.Bold;
            pSegment.ShowLine := False;
            pStrins.Strs.Add(pSegment);
            if pSegment.Bold = 1 then
              Inc(nBold);
            sX := sX + Length(AnsiString(sData)) * 6;
            w := w + Length(AnsiString(sData)) * 6;
          end;
        end;
        if data <> '' then
        begin
          New(pSegment);
          pSegment.Text := data;
          pSegment.offsetX := sX;
          pSegment.indexY := idx;
          pSegment.Image := 0;
          pSegment.Size := 9;
          pSegment.Color := Color;
          pSegment.Bold := 0;
          pSegment.ShowLine := False;
          pStrins.Strs.Add(pSegment);
          w := w + Length(AnsiString(data)) * 6 + 8;
        end
        else
          w := w + 8;
      end;
    end;
    if w > pStrins.Width then
      pStrins.Width := w;
    Inc(idx);
  end;
  if ShowItem and (pStrins.Width <> MAXITEMBOX_WIDTH) then
    pStrins.Width := MAXITEMBOX_WIDTH;

  pStrins.Height := 14 * idx + nBold * 2 + 6;
  Result := pStrins.Height;

  if drawUp then
    pStrins.Y := pStrins.Y - pStrins.Height;
  if drawLeft then
    pStrins.X := pStrins.X - pStrins.Width;
end;

procedure TDxHintMgr.DrawHint(MSurface: TCustomCanvas);
var
  cl: TColor;
  d, dl, dimg: TCustomLockableTexture;
  i, ii, iii, hx, hy, idx, ox, nhy: Integer;
  oSize, oColor: Integer;
  Key, Str, s: string;
  rc: TIntRect;

  pStrins: pTHintStrings;
  pSegment: pTHintTextSegment;
  pts: array [0 .. 4] of TPoint;
  AspFont: TCustomTextureFont;
begin
  if (m_HintList.Count = 0) then
    Exit;
  hx := 0;
  hy := 0;
  for s in m_HintList.Keys do
  begin
    pStrins := m_HintList[s];
    if not pStrins.Show and (GetTickCount - pStrins.LastTick > 10 * 1000) then
    begin // ÐÞ¸ÄÑÓ³ÙÎÊÌâ
      for iii := 0 to pStrins.Strs.Count - 1 do
      Dispose(pTHintTextSegment(pStrins.Strs[iii]));
      pStrins.Strs.Free;
      Dispose(pStrins);
      m_HintList.Remove(s);
    end;
  end;
  for s in m_HintList.Keys do
  begin
    pStrins := m_HintList[s];
    if not pStrins.Show then
      Continue;
    case pStrins.ShowType of
      0:
        begin
          if pStrins.Quad or pStrins.Brush then
            d := g_HintSurface_B
          else
            d := g_WMainImages.Images[394];

          if d = nil then
            Exit;

          if pStrins.Width > d.Width then
            pStrins.Width := d.Width;
          if pStrins.Height > d.Height then
            pStrins.Height := d.Height;

          if pStrins.X + pStrins.Width + 3 > SCREENWIDTH then
            hx := SCREENWIDTH - pStrins.Width - 3
          else
            hx := pStrins.X;

          if pStrins.Y < 0 then
            hy := 0
          else
          begin
            if pStrins.Y + pStrins.Height + 3 > SCREENHEIGHT then
              hy := SCREENHEIGHT - pStrins.Height - 3
            else
              hy := pStrins.Y;
          end;

          if hx < 0 then
            hx := 0;
          MSurface.DrawColorAlpha(hx, hy, IntRectBDS(0, 0, pStrins.Width,
            pStrins.Height), d, GetRGB(162), True, 100);
          // DrawBlend_Mix(MSurface, hx, hy, d, 0, 0, pStrins.Width, pStrins.Height, 0);  //ASP×¢ÊÍ

          if pStrins.Quad then
          begin
            with MSurface do
            begin // ASP×¢ÊÍ
              FrameRect(IntRectBDS(hx, hy, pStrins.Width, pStrins.Height),
                GetRGB(85));
              FrameRect(IntRectBDS(hx - 1, hy - 1, pStrins.Width + 2,
                pStrins.Height + 2), GetRGB(87));
              FrameRect(IntRectBDS(hx - 2, hy - 2, pStrins.Width + 4,
                pStrins.Height + 4), GetRGB(84));
            end;
          end;

          for ii := 0 to pStrins.Strs.Count - 1 do
          begin
            pSegment := pStrins.Strs[ii];
            if pSegment.ShowLine then
            begin
              with MSurface do
              begin // ASP×¢ÊÍ
                // Pen.Color := clGray;
                // MoveTo(hx + pSegment.offsetX + 3, hy + 14 * pSegment.indexY + 4 + 6);
                // LineTo(hx + pSegment.offsetX + pStrins.Width - 3, hy + 14 * pSegment.indexY + 4 + 6);
                // Pen.Color := GetRGB(0);
                // MoveTo(hx + pSegment.offsetX + 3, hy + 14 * pSegment.indexY + 4 + 7);
                // LineTo(hx + pSegment.offsetX + pStrins.Width - 3, hy + 14 * pSegment.indexY + 4 + 7);
                Line(hx + pSegment.offsetX + 3,
                  hy + 14 * pSegment.indexY + 4 + 6,
                  // MoveTo(hx + pSegment.offsetX + 3, hy + 14 * pSegment.indexY + 4 + 6);
                  hx + pSegment.offsetX + pStrins.Width - 3,
                  hy + 14 * pSegment.indexY + 4 + 6, clGray);

                Line(hx + pSegment.offsetX + 3, hy + 14 * pSegment.indexY + 4 +
                  7, hx + pSegment.offsetX + pStrins.Width - 3,
                  hy + 14 * pSegment.indexY + 4 + 7, GetRGB(0));

              end;
            end
            else
            begin
              ox := 0;
              if pSegment.Image > 0 then
              begin
                if pSegment.Text <> '' then
                begin
                  case pSegment.Text[1] of
                    'u':
                      begin
                        dimg := g_wui.Images[pSegment.Image];
                        if dimg <> nil then
                        begin
                          ox := dimg.Width + 2;
                          MSurface.Draw(hx + pSegment.offsetX + 4,
                            hy + 14 * pSegment.indexY + 4, dimg.ClientRect,
                            dimg, True);
                        end;
                      end;
                  end;
                end;
              end;
              if ox = 0 then
              begin
                AspFont := FontManager.GetFont('ËÎÌå', pSegment.Size, []);
                // oSize := MSurface.Canvas.Font.Size;       //ASP×¢ÊÍ
                // MSurface.Canvas.Font.Size := pSegment.Size;

                try
                  if pSegment.Bold = 1 then
                  begin
                    d := FontManager.GetFont('ËÎÌå', pSegment.Size, [fsBold])
                      .TextOut(pSegment.Text);
                    MSurface.DrawBoldText(hx + pSegment.offsetX + 4,
                      hy + 14 * pSegment.indexY + 4, d, pSegment.Color, clBlack)
                    //
                    // MSurface.BoldTextOut(hx + pSegment.offsetX + 4, hy + 14 * pSegment.indexY + 4, pSegment.Color, clBlack, pSegment.Text)
                  end
                  else
                    MSurface.BoldTextOut(hx + pSegment.offsetX + 4,
                      hy + 14 * pSegment.indexY + 4, pSegment.Color, clBlack,
                      pSegment.Text);
                finally

                end;
              end;
            end;
          end;
        end;
      1:
        begin
          d := g_HintSurface_Y;

          if d = nil then
            Exit;

          if pStrins.Width > d.Width then
            pStrins.Width := d.Width;
          if pStrins.Height > d.Height then
            pStrins.Height := d.Height;

          if pStrins.X + pStrins.Width > SCREENWIDTH then
            hx := SCREENWIDTH - pStrins.Width
          else
            hx := pStrins.X;

          if pStrins.Y < 0 then
            hy := 0
          else
            hy := pStrins.Y;

          if hx < 0 then
            hx := 0;

          rc := d.ClientRect;
          rc.Right := rc.Left + pStrins.Width;
          rc.Bottom := rc.Top + pStrins.Height;
          MSurface.Draw(hx, hy, rc, d, True);

          MSurface.FrameRect(IntRectBDS(hx - 1, hy, pStrins.Width,
            pStrins.Height), clBlack);
          with MSurface do
          begin // ASP×¢ÊÍ
            // Line(hx - 1, hy, hx + pStrins.Width, hy, clBlack);
            // Line(hx - 1, hy, hx + pStrins.Width, hy + pStrins.Height - 1, clBlack);
            // Line(hx - 1, hy, hx + hx - 1, hy + pStrins.Height - 1, clBlack);
            // Line(hx - 1, hy, hx - 1, hy, clBlack);
            // Pen.Color := clBlack;
            // MoveTo(hx - 1, hy);
            // LineTo(hx + pStrins.Width, hy);
            // LineTo(hx + pStrins.Width, hy + pStrins.Height - 1);
            // LineTo(hx - 1, hy + pStrins.Height - 1);
            // LineTo(hx - 1, hy);
            // Release;
          end;

          for ii := 0 to pStrins.Strs.Count - 1 do
          begin
            pSegment := pStrins.Strs[ii];
            if pSegment.ShowLine then
            begin
              with MSurface do
              begin // ASP×¢ÊÍ
                // Pen.Color := clGray;
                // MoveTo(hx + pSegment.offsetX + 3, hy + 14 * pSegment.indexY + 4 + 6);
                // LineTo(hx + pSegment.offsetX + pStrins.Width - 3, hy + 14 * pSegment.indexY + 4 + 6);
                // Pen.Color := GetRGB(0);
                // MoveTo(hx + pSegment.offsetX + 3, hy + 14 * pSegment.indexY + 4 + 7);
                // LineTo(hx + pSegment.offsetX + pStrins.Width - 3, hy + 14 * pSegment.indexY + 4 + 7);
                Line(hx + pSegment.offsetX + 3, hy + 14 * pSegment.indexY + 4 +
                  6, hx + pSegment.offsetX + pStrins.Width - 3,
                  hy + 14 * pSegment.indexY + 4 + 6, clGray);

                Line(hx + pSegment.offsetX + 3, hy + 14 * pSegment.indexY + 4 +
                  7, hx + pSegment.offsetX + pStrins.Width - 3,
                  hy + 14 * pSegment.indexY + 4 + 7, GetRGB(0));
              end;
            end
            else
            begin
              // MSurface.Canvas.Font.Color := clBlack;            //ASP×¢ÊÍ
              // MSurface.Canvas.TextOutA(hx + pSegment.offsetX + 4, hy + 14 * pSegment.indexY + 4, pSegment.Text);
              MSurface.TextOut(hx + pSegment.offsetX + 4,
                hy + 14 * pSegment.indexY + 4, pSegment.Text, clBlack);
            end;
          end;
        end;
    end;
  end;
end;

initialization

// g_DxHintMgr := TDxHintMgr.Create;
// g_DxHintMgr.m_AutoClear := False;

// g_DxHintMgr2 := TDxHintMgr.Create;
// g_DxHintMgr3 := TDxHintMgr.Create;

finalization

// g_DxHintMgr.Free;
// g_DxHintMgr2.Free;
// g_DxHintMgr3.Free;

end.

{**************************************************************************}
{ Mini HTML rendering engine                                               }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2008 - 2012                                       }
{            Email : info@tmssoftware.com                                  }
{            Website : http://www.tmssoftware.com/                         }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

{$I TMSDEFS.INC}

procedure PrintBitmap(Canvas:  TCanvas; DestRect:  TRect;  Bitmap:  TBitmap);
var
  BitmapHeader:  pBitmapInfo;
  BitmapImage :  POINTER;
  HeaderSize  :  DWORD;
  ImageSize   :  DWORD;
begin
  GetDIBSizes(Bitmap.Handle, HeaderSize, ImageSize);
  GetMem(BitmapHeader, HeaderSize);
  GetMem(BitmapImage,  ImageSize);
  try
    GetDIB(Bitmap.Handle, Bitmap.Palette, BitmapHeader^, BitmapImage^);
    StretchDIBits(Canvas.Handle,
                  DestRect.Left, DestRect.Top,     // Destination Origin
                  DestRect.Right  - DestRect.Left, // Destination Width
                  DestRect.Bottom - DestRect.Top,  // Destination Height
                  0, 0,                            // Source Origin
                  Bitmap.Width, Bitmap.Height,     // Source Width & Height
                  BitmapImage,
                  TBitmapInfo(BitmapHeader^),
                  DIB_RGB_COLORS,
                  SRCCOPY)
  finally
    FreeMem(BitmapHeader);
    FreeMem(BitmapImage)
  end;
end;


function DirExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function SysImage(Canvas: TCanvas;x,y:Integer;APath:string;large,draw,print:boolean;resfactor:double):TPoint;
var
  SFI: TSHFileInfo;
  i,Err: Integer;
  imglsthandle: THandle;
  rx,ry: Integer;
  bmp: TBitmap;
  r: TRect;
begin
  Val(APath,i,Err);

  FillChar(SFI,Sizeof(SFI),0);

  if (APath <> '') and (Err <> 0) then
  begin
    if FileExists(APath) or DirExists(APath) then
    // If the file or directory exists, just let Windows figure out it's attrs.
      SHGetFileInfo(PChar(APath), 0, SFI, SizeOf(TSHFileInfo),
                    SHGFI_SYSICONINDEX {or OPEN_FLAG[Open] or SELECTED_FLAG[Selected]})
    else
    // File doesn't exist, so Windows doesn't know what to do with it.  We have
    // to tell it by passing the attributes we want, and specifying the
    // SHGFI_USEFILEATTRIBUTES flag so that the function knows to use them.
      SHGetFileInfo(PChar(APath), 0, SFI, SizeOf(TSHFileInfo),
                    SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES {or OPEN_FLAG[Open] or SELECTED_FLAG[Selected]});
    i := SFI.iIcon;
  end;

  if Large then
    imglsthandle := SHGetFileInfo('', 0, SFI, SizeOf(SFI),
                            SHGFI_SYSICONINDEX or SHGFI_LARGEICON)
  else
    imglsthandle := SHGetFileInfo('', 0, SFI, SizeOf(SFI),
                            SHGFI_SYSICONINDEX or SHGFI_SMALLICON);

  ImageList_GetIconSize(imglsthandle,rx,ry);

  Result := Point(rx,ry);

  if Draw and not Print then
    ImageList_Draw(imglsthandle,i,Canvas.Handle,x,y, ILD_TRANSPARENT);

  if Draw and Print then
  begin
    bmp := TBitmap.Create;
    bmp.Width := rx;
    bmp.Height := ry;
    ImageList_Draw(imglsthandle,i,bmp.Canvas.handle,0,0,ILD_NORMAL);
    r.left := x;
    r.top := y;
    r.right := x + Round(rx * ResFactor);
    r.bottom := y + Round(ry * ResFactor);
    PrintBitmap(Canvas,r,bmp);
    bmp.Free;
  end;
end;

procedure DrawHTMLGradient(Canvas: TCanvas; FromColor,ToColor,BorderColor: TColor; Steps: Integer;R:TRect; Direction: Boolean);
var
  diffr,startr,endr: Integer;
  diffg,startg,endg: Integer;
  diffb,startb,endb: Integer;
  iend: Integer;
  rstepr,rstepg,rstepb,rstepw: Real;
  i,stepw: Word;
begin
  if Steps = 0 then
    Steps := 1;

  FromColor := ColorToRGB(FromColor);
  ToColor := ColorToRGB(ToColor);

  startr := (FromColor and $0000FF);
  startg := (FromColor and $00FF00) shr 8;
  startb := (FromColor and $FF0000) shr 16;
  endr := (ToColor and $0000FF);
  endg := (ToColor and $00FF00) shr 8;
  endb := (ToColor and $FF0000) shr 16;

  diffr := endr - startr;
  diffg := endg - startg;
  diffb := endb - startb;

  rstepr := diffr / steps;
  rstepg := diffg / steps;
  rstepb := diffb / steps;

  if Direction then
    rstepw := (R.Right - R.Left) / Steps
  else
    rstepw := (R.Bottom - R.Top) / Steps;

  with Canvas do
  begin
    for i := 0 to Steps - 1 do
    begin
      endr := startr + Round(rstepr*i);
      endg := startg + Round(rstepg*i);
      endb := startb + Round(rstepb*i);
      stepw := Round(i*rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
      begin
        iend := R.Left + stepw + Trunc(rstepw) + 1;
        if iend > R.Right then
          iend := R.Right;
        Rectangle(R.Left + stepw,R.Top,iend,R.Bottom)
      end
      else
      begin
        iend := R.Top + stepw + Trunc(rstepw)+1;
        if iend > r.Bottom then
          iend := r.Bottom;
        Rectangle(R.Left,R.Top + stepw,R.Right,iend);
      end;
    end;

    if BorderColor <> clNone then
    begin
      Brush.Style := bsClear;
      Pen.Color := BorderColor;
      Rectangle(R.Left,R.Top,R.Right,R.Bottom);
    end;
  end;
end;

{
procedure DrawHTMLGradient(Canvas: TCanvas; FromColor,ToColor: TColor; Steps: Integer;R:TRect; Direction: Boolean);
var
  diffr,startr,endr: Integer;
  diffg,startg,endg: Integer;
  diffb,startb,endb: Integer;
  iend: Integer;
  rstepr,rstepg,rstepb,rstepw: Real;
  i,stepw: Word;

begin
  if Steps = 0 then
    Steps := 1;

  FromColor := ColorToRGB(FromColor);
  ToColor := ColorToRGB(ToColor);  

  startr := (FromColor and $0000FF);
  startg := (FromColor and $00FF00) shr 8;
  startb := (FromColor and $FF0000) shr 16;
  endr := (ToColor and $0000FF);
  endg := (ToColor and $00FF00) shr 8;
  endb := (ToColor and $FF0000) shr 16;

  diffr := endr - startr;
  diffg := endg - startg;
  diffb := endb - startb;

  rstepr := diffr / steps;
  rstepg := diffg / steps;
  rstepb := diffb / steps;

  if Direction then
    rstepw := (R.Right - R.Left) / Steps
  else
    rstepw := (R.Bottom - R.Top) / Steps;

  with Canvas do
  begin
    for i := 0 to Steps - 1 do
    begin
      endr := startr + Round(rstepr*i);
      endg := startg + Round(rstepg*i);
      endb := startb + Round(rstepb*i);
      stepw := Round(i*rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
      begin
        iend := R.Left + stepw + Trunc(rstepw) + 1;
        if iend > R.Right then
          iend := R.Right;
        Rectangle(R.Left + stepw,R.Top,iend,R.Bottom)
      end
      else
      begin
        iend := R.Top + stepw + Trunc(rstepw)+1;
        if iend > r.Bottom then
          iend := r.Bottom;
        Rectangle(R.Left,R.Top + stepw,R.Right,iend);
      end;  
    end;
  end;
end;
}

function Text2Color(s:string):tcolor;
begin
  Result := clBlack;

  if (s='clred') then result:=clred else
  if (s='clblack') then result:=clblack else
  if (s='clblue') then result:=clblue else
  if (s='clgreen') then result:=clgreen else
  if (s='claqua') then result:=claqua else
  if (s='clyellow') then result:=clyellow else
  if (s='clfuchsia') then result:=clfuchsia else
  if (s='clwhite') then result:=clwhite else
  if (s='cllime') then result:=cllime else
  if (s='clsilver') then result:=clsilver else
  if (s='clgray') then result:=clgray else
  if (s='clolive') then result:=clolive else
  if (s='clnavy') then result:=clnavy else
  if (s='clpurple') then result:=clpurple else
  if (s='clteal') then result:=clteal else
  if (s='clmaroon') then result:=clmaroon;

  if Result <> clBlack then Exit;

  if (s='clbackground') then result:=clbackground else
  if (s='clactivecaption') then result:=clactivecaption else
  if (s='clinactivecaption') then result:=clinactivecaption else
  if (s='clmenu') then result:=clmenu else
  if (s='clwindow') then result:=clwindow else
  if (s='clwindowframe') then result:=clwindowframe else
  if (s='clmenutext') then result:=clmenutext else
  if (s='clwindowtext') then result:=clwindowtext else
  if (s='clcaptiontext') then result:=clcaptiontext else
  if (s='clactiveborder') then result:=clactiveborder else
  if (s='clinactiveborder') then result:=clinactiveborder else
  if (s='clappworkspace') then result:=clappworkspace else
  if (s='clhighlight') then result:=clhighlight else
  if (s='clhighlighttext') then result:=clhighlighttext else
  if (s='clbtnface') then result:=clbtnface else
  if (s='clbtnshadow') then result:=clbtnshadow else
  if (s='clgraytext') then result:=clgraytext else
  if (s='clbtntext') then result:=clbtntext else
  if (s='clinactivecaptiontext') then result:=clinactivecaptiontext else
  if (s='clbtnhighlight') then result:=clbtnhighlight else
  if (s='cl3ddkshadow') then result:=clgraytext else
  if (s='cl3dlight') then result:=cl3dlight else
  if (s='clinfotext') then result:=clinfotext else
  if (s='clinfobk') then result:=clinfobk;
end;

function HexVal(s:string): Integer;
var
  i,j: Integer;
begin
  if Length(s) < 2 then
  begin
    Result := 0;
    Exit;
  end;

  if s[1] >= 'A' then
    i := ord(s[1]) - ord('A') + 10
  else
    i := ord(s[1]) - ord('0');

  if s[2] >= 'A' then
    j := ord(s[2]) - ord('A') + 10
  else
    j := ord(s[2]) - ord('0');

  Result := i shl 4 + j;
end;

function Hex2Color(s:string): TColor;
var
  r,g,b: Integer;
begin
  r := Hexval(Copy(s,2,2));
  g := Hexval(Copy(s,4,2)) shl 8;
  b := Hexval(Copy(s,6,2)) shl 16;
  Result := TColor(b + g + r);
end;

function IPos(su,s:string):Integer;
begin
  Result := Pos(UpperCase(su),UpperCase(s));
end;

function IStrToInt(s:string):Integer;
var
  Err,Res: Integer;
begin
  Val(s,Res,Err);
  Result := Res;
end;

function DBTagStrip(s:string):string;
var
  i,j: Integer;
begin
  i := Pos('<#',s);
  if i > 0 then
  begin
    Result := Copy(s,1,i - 1);
    Delete(s,1,i);
    j := Pos('>',s);
    if j > 0 then
      Delete(s,j,1);
    Result := Result + s;
  end
  else
    Result := s;
end;

function CRLFStrip(s:string;break:boolean):string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
  begin
    {$IFDEF DELPHI_UNICODE}
    if not (CharInSet(s[i], [#13,#10])) then
    {$ENDIF}
    {$IFNDEF DELPHI_UNICODE}
    if not (s[i] in [#13,#10]) then
    {$ENDIF}
      Result := Result + s[i]
    else
    begin
      if (s[i] = #13) and break then
        Result := Result + '<BR>';
    end;
  end;
end;

function VarPos(su,s:string;var Res:Integer):Integer;
begin
  Res := Pos(su,s);
  Result := Res;
end;

function TagReplaceString(const Srch,Repl:string;var Dest:string):Boolean;
var
  i: Integer;
begin
  i := IPos(srch,dest);
  if i > 0 then
  begin
    Result := True;
    Delete(Dest,i,Length(Srch));
    Dest := Copy(Dest,1,i-1) + Repl + Copy(Dest,i,Length(Dest));
  end
  else
    Result := False;
end;

{$WARNINGS OFF}
function HTMLDrawGDIP(g:TGPGraphics; AFont: TFont; s:string; fr:TRect;
                    FImages: TCustomImageList;
                    XPos,YPos,FocusLink,HoverLink,ShadowOffset: Integer;
                    CheckHotSpot,CheckHeight,Print,Selected,Blink,HoverStyle,WordWrap: Boolean;
                    ResFactor:Double;
                    URLColor,HoverColor,HoverFontColor,ShadowColor:TColor;
                    var AnchorVal,StripVal,FocusAnchor: string;
                    var XSize,YSize,HyperLinks,MouseLink: Integer;
                    var HoverRect:TRect;ic: TAdvGDIPPictureCache; pc: TGDIPPictureContainer;LineSpacing: Integer): Boolean;
var
  su,nsu: string;
  r,dr,hr,rr,er: TRect;
  htmlwidth,htmlheight,txtheight: Integer;
  Align: TAlignment;
  PIndent: Integer;
  OldFont: TFont;
  CalcFont: TFont;
  DrawFont: TFont;
  OldCalcFont: TFont;
  OldDrawFont: TFont;
  Hotspot, ImageHotspot: Boolean;
  Anchor,OldAnchor,MouseInAnchor,Error: Boolean;
  bgcolor,paracolor,hvrcolor,hvrfntcolor,pencolor,blnkcolor,hifcol,hibcol: TColor;
  LastAnchor,OldAnchorVal,HTitle: string;
  IMGSize: TPoint;
  isSup,isSub,isPara,isShad: Boolean;
  subh,suph,imgali,srchpos,hlcount,licount: Integer;
  hrgn,holdfont: THandle;
  ListIndex: Integer;
  dtp: TDrawTextParams;
  Invisible: Boolean;
  FoundTag: Boolean;
  {new for editing}
  nnFit: Integer;
  nnSize: TSize;
  inspoint: Integer;
  nndx: Pointer;
  AltImg,ImgIdx,OldImgIdx: Integer;
  DrawStyle: DWord;
  Col1,Col2: TColor;
  ofsx,newofsx: integer;
  GPFont: TGPFont;
  GPPen: TGPPen;
  GPBrush: TGPSolidBrush;
  Font: TFont;
  Brush: TBrush;
  ACanvas: TCanvas;
  ufl: boolean;
  ColL,ColB: TColor;
  LType,LName: string;

  function HTMLDrawLine(g: TGPGraphics;var s:string;r: TRect;Calc:Boolean;
                        var w,h,subh,suph,imgali:Integer;var Align:TAlignment; var PIndent: Integer;
                        XPos,YPos:Integer;var Hotspot,ImageHotSpot:Boolean;OffsetX: integer; var NewOffsetX: integer):string;
  var
    su,Res,TagProp,Prop,AltProp,Tagp,LineText:string;
    cr: TRect;
    linebreak,imgbreak,linkbreak: Boolean;
    th,sw,indent,err,bmpx,bmpy,oldh: Integer;
    TagPos,SpacePos,o,l: Integer;
    bmp: TAdvGDIPPicture;
    ABitmap: TBitmap;
    NewColor,NewColorTo: TColor;
    TagWidth,TagHeight,WordLen,WordLenEx,WordWidth: Integer;
    TagChar: Char;
    LengthFits, SpaceBreak: Boolean;
    bkColor: TColor;

  begin
    Result := '';
    LineText := '';
    WordWidth := 0;

    r.Bottom := r.Bottom - Subh;

    w := 0;
    sw := 0;

    LineBreak := False;
    ImgBreak := False;
    LinkBreak := False;
    HotSpot := False;
    ImageHotSpot := False;
    HTitle := '';

    cr := r;
    res := '';

    if not Calc then
      cr.Left := cr.Left + OffsetX;

    while (Length(s) > 0) and not LineBreak and not ImgBreak do
    begin
      // outputdebugstring(pchar('*'+s+'*'));
      // get next word or till next HTML tag
      TagPos := Pos('<',s);

      if WordWrap then
        SpacePos := Pos(' ',s)
      else
        SpacePos := 0;

      if (Tagpos > 0) and ((SpacePos > TagPos) or (SpacePos = 0)) then
      begin
        su := Copy(s,1,TagPos - 1);
      end
      else
      begin
        if SpacePos > 0 then
          su := Copy(s,1,SpacePos)
        else
          su := s;
      end;

      //outputdebugstring(pchar('.'+su+'.'));

      {$IFDEF TMSDEBUG}
      DbgMsg(su+ '.');
      {$ENDIF}

      WordLen := Length(su);

      while Pos('&nbsp;',su) > 0 do
      begin
        TagReplacestring('&nbsp;',' ',su);
      end;

      while Pos('&lt;',su) > 0 do
      begin
        TagReplacestring('&lt;','<',su);
      end;

      while Pos('&gt;',su) > 0 do
      begin
        TagReplacestring('&gt;','>',su);
      end;

      WordLenEx := Length(su);

      if WordLen > 0 then
      begin
        th := g.TextHeight(su, font);

        if isSub and (subh < (th shr 2)) then subh := th div 2;
        if isSup and (suph < (th shr 2)) then suph := th div 2;

        if th > h then
          h := th;

        StripVal := StripVal + su;

        if Invisible then
          Delete(s,1,WordLen);

        bkColor := Brush.Color;
        if Brush.Style = bsClear then
         bkColor := clNone;

        if not Invisible then
        begin
          // draw mode
          if not Calc then
          begin
            if isSup then
            begin
              cr.Bottom := cr.Bottom - suph;
              cr.Top := cr.Top - suph div 2;
            end;
            if isSub then
            begin
              cr.Bottom := cr.Bottom + subh;
              cr.Top := cr.top + subh div 2;
            end;

            cr.Bottom := cr.Bottom - imgali;

            if isShad then
            begin
              OffsetRect(cr,ShadowOffset,ShadowOffset);
              NewColor := Font.Color;
              Font.Color := ShadowColor;

              g.DrawText(su, WordLenEx, cr, Font, DrawStyle and not DT_EXTERNALLEADING, bkColor);
              Offsetrect(cr,-ShadowOffset,-ShadowOffset);
              Font.Color := NewColor;
            end;

            if (su = ' ') then
            begin
              g.DrawText(su, WordLenEx, cr, Font, DrawStyle or DT_CALCRECT);
              //cr.Right := cr.Left + 0
            end
            else
            begin
              DrawStyle := DrawStyle and NOT DT_CALCRECT;
              ufl := false;

              if (fsUnderline in Font.Style) then
              begin
                nsu := s;
                Delete(nsu,1,length(su));

                if pos(' ',nsu) > 0 then
                begin
                  nsu := Copy(nsu,1,pos(' ',nsu));

                  if pos('<',nsu) > 0 then
                    nsu := Copy(nsu,1,pos('<',nsu)- 1 );
                end;

                g.DrawText(su + nsu, WordLenEx + length(nsu), cr, Font, DrawStyle or DT_CALCRECT);

                if {(cr.Right < fr.Right + 2) and} (nsu <> '') then
                begin
                  if (pos(' ',s) > 0) and (pos(' ',s) < pos('</A', uppercase(s))) then
                    g.DrawText(su  + '_', WordLenEx + 1, cr, Font, DrawStyle and not DT_EXTERNALLEADING, bkColor)
                  else
                    g.DrawText(su, WordLenEx, cr, Font, DrawStyle and not DT_EXTERNALLEADING, bkColor);

                  ufl := true;
                end;
              end;

              if not ufl then
                g.DrawText(su, WordLenEx, cr, Font, DrawStyle and not DT_EXTERNALLEADING, bkColor);

              g.DrawText(su, WordLenEx, cr, Font, DrawStyle or DT_CALCRECT);

              //cr.Right := cr.Right - 1;
            end;

            //OutputDebugString(pchar('drawrect for -'+ su +'- = ['+inttostr(cr.Left)+':'+inttostr(cr.Top)+'] ['+inttostr(cr.right)+':'+inttostr(cr.bottom)+'] @ ['+inttostr(xpos)+':'+inttostr(ypos)));

            if Anchor and (Hyperlinks - 1 = FocusLink) then
              FocusAnchor := LastAnchor;

            {$IFDEF TMSDEBUG}
            if Anchor then
              OutputDebugString(pchar('drawrect for '+anchorval+' = ['+inttostr(cr.Left)+':'+inttostr(cr.Top)+'] ['+inttostr(cr.right)+':'+inttostr(cr.bottom)+'] @ ['+inttostr(xpos)+':'+inttostr(ypos)));
            {$ENDIF}

            if Error then
            begin
              l := (cr.Left div 2) * 2;

              if (l mod 4)=0 then o := 2 else o := 0;

              GPPen := TGPPen.Create(MakeColor(255,clRed),1);

              while l < cr.Right do
              begin
                if o = 2 then o := 0 else o := 2;
                g.DrawLine(GPPen,l,r.Bottom - 3 + o - 1, l + 2,r.bottom - 3 - o + 1);
                Inc(l,2);
              end;
              if o = 2 then o := 0 else o := 2;
              g.DrawLine(GPPen,l,r.Bottom - 3 + o - 1, l + 2,r.bottom - 3 - o + 1);
              GPPen.Free;
            end;

            cr.Left := cr.Right;
            cr.Right := r.Right;
            cr.Bottom := r.Bottom;
            cr.Top := r.Top;
          end
        else
          begin
            cr := r; //reinitialized each time !

            if (su = ' ') then
              cr.Right := cr.Left + 0
            else
              g.DrawText(su, WordLenEx, cr, Font, DrawStyle or DT_CALCRECT);

            //Calculated text width
            WordWidth := cr.Right - cr.Left;
            w := w + WordWidth;

            if (XPos - cr.Left  >= w - WordWidth) and (XPos - cr.Left <= w) and Anchor then
            begin
              HotSpot := True;
              if (YPos > cr.Top) and (YPos < cr.Top + g.TextHeight('gh', font)) then
              begin
                Anchorval := LastAnchor;
                FocusAnchor := Htitle;
                MouseInAnchor := True;
              end;
            end;
          end;

          LengthFits := (w <= r.Right - r.Left - OfsX) or (r.Right - r.Left - OfsX <= WordWidth);

          if not LengthFits and
            ((Length(LineText) > 0) and (LineText[Length(LineText)] <> ' ')) then
            LengthFits := True;

          LineText := LineText + su;

          if LengthFits or not WordWrap then
          begin
            Res := Res + Copy(s,1,WordLen);

            //if not LengthFits and Calc and (LineText <> su) then
            //  s := '';

            Delete(s,1,WordLen);

            if Length(su) >= WordLen then
            begin
              //if System.Copy(su, WordLen, 1) = ' ' then
              //  sw := g.TextWidth(' ', Font)
              //else
              //  sw := 0;
            end
            else
              sw := 0;
          end
          else
          begin
            LineBreak := True;
            w := w - WordWidth;
          end;
        end;
      end;

      TagPos := Pos('<',s);

      if (TagPos = 1) and (Length(s) <= 2) then
        s := '';

      if not LineBreak and (TagPos = 1) and (Length(s) > 2) then
      begin
        if (s[2] = '/') and (Length(s) > 3) then
        begin
          case UpCase(s[3]) of
          'A':begin
                if (not HoverStyle or (Hoverlink = Hyperlinks)) and not Calc then
                begin
                  Font.Style := Font.Style - [fsUnderline];
                  if Hovercolor <> clNone then
                  begin
                    Brush.Color := HvrColor;
                    if HvrColor = clNone then
                      Brush.Style := bsClear;
                  end;
                  if HoverFontColor <> clNone then
                    Font.Color := HoverFontColor;
                end;

                if not Selected then
                  Font.Color := Oldfont.Color;

                Anchor := False;

                if MouseInAnchor then
                begin
                  hr.Bottom := r.Bottom;
                  hr.Right := r.Left + w;
                  if r.Top <> hr.Top then
                  begin
                    hr.Left := r.Left;
                    hr.Top := r.Top;
                  end;
                  
                  HoverRect := hr;
                  MouseLink := HyperLinks;
                  {$IFDEF TMSDEBUG}
                  DbgRect('hotspot anchor '+lastanchor,hr);
                  {$ENDIF}
                  MouseInAnchor := False;
                end;

                if Focuslink = Hyperlinks - 1 then
                begin
                  rr.Right := cr.Left;
                  rr.Bottom := cr.Bottom - ImgAli;
                  rr.Top := rr.Bottom - g.TextHeight('gh', font);
                  InflateRect(rr,1,0);
                  ///if not Calc then Canvas.DrawFocusRect(rr);
                end;
              end;
          'E':begin
                if not Calc then
                  Error := False;
              end;
          'B':begin
                if s[4] <> '>' then
                  Font.Color := OldFont.Color
                else
                  Font.Style := Font.Style - [fsBold];
              end;
          'S':begin
                TagChar := UpCase(s[4]);

                if (TagChar = 'U') then
                begin
                  isSup := False;
                  isSub := False;
                end
                else
                 if (TagChar = 'H') then
                  isShad := False
                 else
                  Font.Style := Font.Style - [fsStrikeOut];
              end;
          'F':begin
                Font.Name := OldFont.Name;
                Font.Size := OldFont.Size;
                if not Calc and not Selected then
                begin
                  Font.Color := OldFont.Color;
                  Brush.Color := BGColor;
                  if BGColor = clNone then
                  begin
                    Brush.Style := bsClear;
                  end;
                end;
              end;
          'H':begin
                if not Calc then
                begin
                  Font.Color := hifCol;
                  Brush.Color := hibCol;
                  if hibCol = clNone then
                    Brush.Style := bsClear;
                end;
              end;
          'I':begin
                Font.Style := Font.Style - [fsItalic];
              end;
          'L':begin
                LineBreak := True;
              end;
          'O':begin
                NewOffsetX := 0;
              end;
          'P':begin
                LineBreak := True;
                if not Calc then
                begin
                  Brush.Color := ParaColor;
                  if ParaColor = clNone then
                    Brush.Style := bsClear;
                  isPara := false;
                end;
              end;
          'U':begin
                if (s[4] <> '>') and (ListIndex > 0) then
                  Dec(Listindex)
                else
                  Font.Style := Font.Style - [fsUnderline];
              end;
          'Z':Invisible := False;
          end;
        end
        else
        begin
          case Upcase(s[2]) of
          'A':begin
                { only do this when at hover position in xpos,ypos }
                if (FocusLink = HyperLinks) and not Calc then
                begin
                  rr.Left := cr.Left;
                  rr.Top := cr.Top;
                end;

                Inc(HyperLinks);
                if (not HoverStyle or (Hoverlink = HyperLinks)) and not Calc then
                begin
                  Font.Style := Font.Style + [fsUnderline];
                  if (Hovercolor <> clNone) and not Calc then
                  begin
                    HvrColor := Brush.Color;

                    if Brush.Style = bsClear then
                      HvrColor := clNone;
                    Brush.Color := HoverColor;
                  end;

                  if HoverFontColor <> clNone then
                  begin
                    hvrfntcolor := Font.Color;
                    Font.Color := HoverFontColor;
                  end;
                end;

                if not Selected and ((HoverFontColor = clNone) or (HoverLink <> HyperLinks) or not HoverStyle) then
                  Font.Color := URLColor;

                TagProp := Uppercase(Copy(s,3,Pos('>',s) - 1));  // <A href="anchor">

                if (VarPos('HREF',TagProp,TagPos) > 0) then
                begin
                  TagProp := Copy(s,3,Pos('>',s) - 1);
                  Prop := Copy(TagProp,TagPos + 4,Length(TagProp));
                  Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                  Prop := Copy(Prop,1,Pos('"',Prop) - 1);
                  LastAnchor := Prop;
                  Anchor := True;
                end;

                TagProp := Uppercase(Copy(s,3,Pos('>',s) - 1));  // <A href="anchor">

                if (VarPos('TITLE',TagProp,TagPos) > 0) then
                begin
                  TagProp := Copy(s,3,Pos('>',s) - 1);  // <A href="anchor">
                  Prop := Copy(TagProp,TagPos + 5,Length(TagProp));
                  Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                  Prop := Copy(Prop,1,Pos('"',Prop) - 1);
                  HTitle := Prop;
                end;

                hr.Left := w;
                hr.Top := r.Top;
              end;
          'B':begin
                TagChar := Upcase(s[3]);
                case TagChar of
                '>': Font.Style := Font.Style + [fsBold]; // <B> tag
                'R': // <BR> tag
                   begin
                    LineBreak := true;
                    StripVal := StripVal + #13;
                   end;
                'L': if not Blink then
                   Font.Color := BlnkColor; // <BLINK> tag
                'O':
                  begin
                    Res := Res + Copy(s,1,pos('>',s));
                    if not Calc and not Selected then
                    begin
                      TagProp := Uppercase(Copy(s,6,pos('>',s)-1));

                      if VarPos('BGCOLOR',TagProp,TagPos) > 0 then
                      begin
                        Prop := Copy(TagProp,TagPos + 5,Length(TagProp));
                        Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                        Prop := Copy(Prop,1,Pos('"',Prop) - 1);

                        NewColor := clNone;

                        if Length(Prop) > 0 then
                        begin
                          if Prop[1] = '#' then
                            NewColor := Hex2Color(Prop)
                          else
                            NewColor := Text2Color(AnsiLowerCase(prop));
                        end;

                        if VarPos('BGCOLORTO',TagProp,TagPos) > 0 then
                        begin
                          Prop := Copy(TagProp,TagPos + 5,Length(TagProp));
                          Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                          Prop := Copy(Prop,1,Pos('"',Prop) - 1);
                          NewColorTo := clNone;

                          if Length(Prop) > 0 then
                          begin
                            if Prop[1] = '#' then
                              NewColorTo := Hex2Color(Prop)
                            else
                              NewColorTo := Text2Color(AnsiLowerCase(prop));
                          end;

                          Prop := 'H';
                          if VarPos('DIR',TagProp,TagPos) > 0 then
                          begin
                            Prop := Copy(TagProp,TagPos + 3,Length(TagProp));
                            Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                            Prop := Copy(Prop,1,Pos('"',Prop) - 1);
                          end;

                          //Pen.Color := Newcolor;
                          ///DrawHTMLGradient(Canvas,NewColor,NewColorTo,clNone,64,Rect(fr.left,fr.top,fr.right,fr.bottom),Prop = 'H');
                          Brush.Style := bsClear
                        end
                        else
                        begin
                          BGColor := Brush.Color;
                          Brush.color := NewColor;
                          //PenColor := Pen.Color;
                          //Pen.Color := Newcolor;
                          //Canvas.Rectangle(fr.left - 2,fr.top,fr.right,fr.bottom);
                          //Pen.Color := PenColor;
                        end;
                      end;
                    end;
                  end;
                end;
              end;
          'E':begin
                if not Calc then
                  Error := True;
              end;
          'H':begin
                case Upcase(s[3]) of
                'R':
                begin
                  LineBreak := True;
                  if not Calc then
                  begin
                    GPPen := TGPPen.Create(MakeColor(255, clBlack), 1);
                    g.DrawLine(GPPen, r.left,cr.bottom + 1,r.right,cr.bottom + 1);
                    GPPen.Free;
                  end;
                end;
                'I':
                begin
                  if not Calc then
                  begin
                    hifCol := Font.Color;
                    hibCol := Brush.Color;
                    if Brush.Style = bsClear then
                      hibCol := clNone;

                    Brush.Color := clHighLight;
                    Font.Color := clHighLightText;
                  end;
                end;
                end;
              end;
          'I':begin
                TagChar := Upcase(s[3]);

                if TagChar = '>' then // <I> tag
                  Font.Style := Font.Style + [fsItalic]
                else
                if TagChar = 'N' then  // <IND> tag
                begin
                  TagProp := Copy(s,3,pos('>',s) - 1);

                  Prop := Copy(TagProp,ipos('x',TagProp) + 2,Length(TagProp));
                  Prop := Copy(Prop,Pos('"',Prop) + 1,Length(prop));
                  Prop := Copy(Prop,1,Pos('"',Prop) - 1);

                  val(Prop,indent,err);
                  if err = 0 then
                  begin
                    if indent > w then
                     begin
                       w := Indent;
                       cr.left := fr.left + Indent;
                     end;
                  end;
                end
                else
                  if TagChar = 'M' then
                  begin
                    inc(ImgIdx);

                    TagProp := Copy(s,3,Pos('>',s) - 1);
                    Prop := Copy(TagProp,Pos('SRC',Uppercase(TagProp)) + 4,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',Prop) - 1);

                    TagProp := Uppercase(TagProp);

                    if (Pos('ALT',TagProp) > 0) and (AltImg = ImgIdx) then
                    begin
                      Prop := Copy(TagProp,Pos('ALT',TagProp) + 4,Length(TagProp));
                      Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                      Prop := Copy(Prop,1,Pos('"',Prop) - 1);
                    end;

                    TagWidth := 0;
                    TagHeight := 0;

                    if Pos('WIDTH',TagProp) > 0 then
                    begin
                      Tagp := Copy(TagProp,Pos('WIDTH',TagProp) + 6,Length(TagProp));
                      Tagp := Copy(Tagp,Pos('"',tagp) + 1,Length(Tagp));
                      Tagp := Copy(Tagp,1,Pos('"',tagp) - 1);
                      Val(Tagp,TagWidth,Err);
                    end;

                    if Pos('HEIGHT',TagProp) > 0 then
                    begin
                      Tagp := Copy(TagProp,ipos('HEIGHT',TagProp) + 7,Length(TagProp));
                      Tagp := Copy(Tagp,pos('"',Tagp) + 1,Length(Tagp));
                      Tagp := Copy(Tagp,1,pos('"',Tagp) - 1);
                      Val(Tagp,TagHeight,Err);
                    end;

                    IMGSize.x := 0;
                    IMGSize.y := 0;


                    if Pos('IDX:',Uppercase(Prop)) > 0 then
                    begin
                      Delete(Prop,1,4);
                      if Assigned(FImages) and (IStrToInt(Prop) < FImages.Count) then
                      begin
                        IMGSize.x := FImages.Width;
                        IMGSize.y := FImages.Height;

                        if not Calc and not Print then
                        begin
                          ACanvas := TCanvas.Create;
                          ACanvas.Handle := g.GetHDC;
                          FImages.Draw(ACanvas,cr.Left,cr.Top,IStrToInt(Prop),True);
                          g.ReleaseHDC(ACanvas.Handle);
                          ACanvas.Free;
                        end;

                      end;
                    end;

                    if Pos('SSYS:',Uppercase(Prop)) > 0 then
                    begin
                      Delete(Prop,1,5);

                      ACanvas := TCanvas.Create;
                      ACanvas.Handle := g.GetHDC;
                      IMGSize := SysImage(ACanvas,cr.Left,cr.Top,Prop,False,not Calc,Print,ResFactor);
                      g.ReleaseHDC(ACanvas.Handle);
                      ACanvas.Free;

                      IMGSize.x := IMGSize.X;
                      IMGSize.y := IMGSize.Y;
                    end;

                    if Pos('LSYS:',Uppercase(Prop)) > 0 then
                    begin
                      Delete(Prop,1,5);

                      ACanvas := TCanvas.Create;
                      ACanvas.Handle := g.GetHDC;
                      IMGsize := SysImage(ACanvas,cr.Left,cr.Top,Prop,True,not Calc,Print,ResFactor);
                      g.ReleaseHDC(ACanvas.Handle);
                      ACanvas.Free;

                      IMGSize.x := IMGSize.X;
                      IMGSize.y := IMGSize.Y;
                    end;

                    bmp := nil;

                    if (Pos(':',Prop) = 0) and Assigned(pc) then
                    begin
                      bmp := pc.FindPicture(Prop);
                    end;

                      if bmp <> nil then
                      begin
                        if not bmp.Empty then
                        begin

                          if not Calc {and not Print} then
                          begin
                            if (TagWidth > 0) and (TagHeight > 0) then
                            begin
                              bmp.GDIPDraw(g, Rect(cr.Left,cr.Top,cr.Left + TagWidth,cr.Top + TagHeight));
                            end
                            else
                            begin
                              bmp.GetImageSizes;
                              bmp.GDIPDraw(g, Rect(cr.Left, cr.Top, cr.Left + bmp.Width, cr.Top + bmp.Height));
                            end;
                          end;

                          if (TagWidth > 0) and (TagHeight > 0) then
                          begin
                            IMGSize.x := TagWidth;
                            IMGSize.y := TagHeight;
                          end
                          else
                          begin
                            bmp.GetImageSizes;
                            IMGSize.x := bmp.Width;
                            IMGSize.y := bmp.Height;
                          end;
                        end;

                      end;

                    if (XPos - r.Left > w) and (XPos - r.Left < w + IMGSize.x) and
                       (YPos > cr.Top) and (YPos < cr.Top + IMGSize.Y) and Anchor then
                    begin
                      ImageHotSpot := True;
                      AnchorVal := LastAnchor;
                      if HTitle <> '' then
                        FocusAnchor := HTitle;
                      AltImg := ImgIdx;
                    end;

                    oldh := h;

                    if (w + IMGSize.x > r.Right-r.Left) and
                       (IMGSize.x < r.Right - r.Left) then
                    begin
                      ImgBreak := True;
                    end
                    else
                      begin
                        w := w + IMGSize.x;
                        cr.left := cr.left + IMGSize.x;
                        if IMGSize.y > h then
                          h := IMGSize.y;
                      end;

                    if Pos('ALIGN',TagProp) > 0 then
                    begin
                      if Pos('"TOP',TagProp) > 0 then
                      begin
                        ImgAli := h - g.TextHeight('gh', font);
                      end
                      else
                      begin
                        if Pos('"MIDDLE',TagProp) > 0 then
                          ImgAli := (h - g.TextHeight('gh', font)) div 2;
                      end;
                    end;

                    if (Pos('WRAP',TagProp) > 0) then
                    begin
                      h := g.TextHeight('gh', font);
                      ImgAli := 0;
                    end;
                  end;
                end;
          'L':begin
                NewColor := clNone;
                LType := '';

                if (VarPos('>',s,TagPos)>0) then
                begin
                  TagProp := Uppercase(Copy(s,3,TagPos-1));

                  if VarPos('TYPE',TagProp,TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp,TagPos+4,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',prop)+1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',prop)-1);
                    LType := Prop;
                  end;

                  if VarPos('COLOR',TagProp,TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp,TagPos+5,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',prop)+1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',prop)-1);

                    if Length(Prop) > 0 then
                    begin
                      if Prop[1] = '#' then
                        NewColor := Hex2Color(Prop)
                      else
                        NewColor := Text2Color(AnsiLowerCase(prop));
                    end;
                  end;

                  if VarPos('NAME',TagProp,TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp,TagPos+4,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',prop)+1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',prop)-1);
                    LName := Prop;
                  end;
                end;

                w := w + 12 * ListIndex;

                if Linkbreak then
                  Imgbreak := True else Linkbreak := True;

                cr.left := cr.left + 12 * (ListIndex - 1);

                if not calc and not Invisible then
                begin
                  if LType = '' then
                  begin
                    if NewColor <> clNone then
                    begin
                      ColL := Font.Color;
                      Font.Color := NewColor;
                    end;

                    Prop := Font.Name;
                    Font.Name := 'Symbol';

                    if Odd(ListIndex) then
                      g.DrawText('·',1,cr,font,0)
                    else
                      g.DrawText('o',1,cr,font,0);

                    Font.Name := prop;

                    if NewColor <> clNone then
                      Font.Color := ColL;
                  end
                  else
                  begin
                    if LTYPE = 'SQUARE' then
                    begin
                      GPBrush := TGPSolidBrush.Create(MakeColor(255,NewColor));
                      g.FillRectangle(GPBrush,cr.Left, cr.Top + 2, 8,8);
                      GPBrush.Free;
                    end;

                    if LTYPE = 'CIRCLE' then
                    begin
                      GPBrush := TGPSolidBrush.Create(MakeColor(255,NewColor));
                      g.FillEllipse(GPBrush,cr.Left, cr.Top + 2, 8,8);
                      GPBrush.Free;
                    end;

                    if (LTYPE = 'IMAGE') and (LName <> '') then
                    begin
                      bmp := nil;

                      if Assigned(pc) then
                      begin
                        bmp := pc.FindPicture(LName);
                      end;

                      if bmp <> nil then
                      begin
                        if not bmp.Empty then
                        begin
                          bmp.GetImageSizes;
                          bmp.GDIPDraw(g, Rect(cr.Left, cr.Top + 2, cr.Left + bmp.Width, cr.Top + 2 + bmp.Height));
                          if bmp.WIdth > 12 then
                            cr.Left := cr.Left + (bmp.Width - 12);
                        end;
                      end;
                    end;
                  end;
                end;

                cr.Left := cr.Left + 12;
              end;
          'U':begin
                if s[3] <> '>' then
                begin
                  Inc(ListIndex);
                  LineBreak := true;
                end
                else
                  Font.Style := Font.Style + [fsUnderline];
              end;
          'O':begin
                TagChar := Upcase(s[3]);
                if TagChar = 'F' then  // <OFS> tag
                begin
                  TagProp := Copy(s,3,pos('>',s) - 1);
                  Prop := Copy(TagProp,ipos('x',TagProp) + 2,Length(TagProp));
                  Prop := Copy(Prop,Pos('"',Prop) + 1,Length(prop));
                  Prop := Copy(Prop,1,Pos('"',Prop) - 1);
                  val(Prop,NewOffsetX,err);
                  cr.Left := NewOffsetX;
                  w := NewOffsetX;
                end
              end;
          'P':begin
                if (VarPos('>',s,TagPos)>0) then
                begin
                  TagProp := Uppercase(Copy(s,3,TagPos-1));

                  if VarPos('ALIGN',TagProp,TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp,TagPos+5,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',prop)+1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',prop)-1);

                    if Pos('RIGHT',Prop) > 0 then Align := taRightJustify;
                    if Pos('LEFT',Prop) > 0 then Align := taLeftJustify;
                    if Pos('CENTER',Prop) > 0 then Align := taCenter;
                  end;

                  if VarPos('INDENT',TagProp,TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp,TagPos+6,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',prop)+1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',prop)-1);
                    PIndent := IStrToInt(Prop);
                  end;

                  if VarPos('BGCOLOR',TagProp,TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp,TagPos + 5,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',Prop) - 1);

                    if VarPos('BGCOLORTO',TagProp,TagPos) > 0 then
                    begin
                      Prop := Copy(TagProp,TagPos + 5,Length(TagProp));
                      Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                      Prop := Copy(Prop,1,Pos('"',Prop) - 1);
                      NewColorTo := clNone;

                      if Length(Prop) > 0 then
                      begin
                        if Prop[1] = '#' then
                          NewColorTo := Hex2Color(Prop)
                        else
                          NewColorTo := Text2Color(AnsiLowerCase(prop));
                      end;
                      if not Calc then
                      begin
                        isPara := True;
                        //Pen.Color := Newcolor;
                        //DrawHTMLGradient(Canvas,NewColor,NewColorTo,clNone,64,Rect(fr.left,r.top,fr.right,r.bottom+2),true);
                        Brush.Style := bsClear
                      end;
                    end
                    else
                    begin
                      if not Calc then
                      begin
                        isPara := True;
                        //paracolor := Brush.Color;
                        //if Brush.Style = bsClear then
                        //  ParaColor := clNone;
                        //Brush.color := NewColor;
                        //PenColor := Pen.Color;
                        //Pen.Color := Newcolor;
                        ///Canvas.Rectangle(fr.left,r.top,fr.right,r.bottom);
                      end;
                    end;  
                  end;
                end;
            end;
        'F':begin
              if (VarPos('>',s,TagPos)>0) then
              begin
                TagProp := UpperCase(Copy(s,6,TagPos-6));

                if (VarPos('FACE',TagProp,TagPos) > 0) then
                begin
                  Prop := Copy(TagProp,TagPos+4,Length(TagProp));
                  Prop := Copy(prop,pos('"',prop)+1,Length(prop));
                  Prop := Copy(prop,1,pos('"',prop)-1);
                  Font.Name := Prop;
                end;

                if (VarPos(' COLOR',TagProp,TagPos) > 0) and not Selected then
                begin
                  Prop := Copy(TagProp,TagPos+6,Length(TagProp));
                  Prop := Copy(Prop,Pos('"',prop)+1,Length(prop));
                  Prop := Copy(Prop,1,Pos('"',prop)-1);
                  //oldfont.color:=Canvas.font.color;

                  if Length(Prop) > 0 then
                  begin
                    if Prop[1] = '#' then
                      Font.color := Hex2Color(Prop)
                    else
                      Font.Color := Text2Color(AnsiLowerCase(prop));
                  end;

                end;

                if (VarPos('BGCOLOR',TagProp,TagPos)>0) and not Calc and not Selected then
                begin
                  Prop := Copy(TagProp,TagPos+7,Length(TagProp));
                  Prop := Copy(prop,pos('"',prop)+1,Length(prop));
                  Prop := Copy(prop,1,pos('"',prop)-1);
                  BGColor := Brush.Color;

                  if Brush.Style = bsClear then
                    bgcolor := clNone;

                  if Length(Prop) > 0 then
                  begin
                    if Prop[1] = '#' then
                      Brush.Color := Hex2Color(Prop)
                    else
                      Brush.Color := Text2Color(AnsiLowerCase(prop));
                  end;

                end;

                if (VarPos('SIZE',TagProp,TagPos)>0) then
                begin
                  Prop := Copy(TagProp,TagPos+4,Length(TagProp));
                  Prop := Copy(Prop,Pos('=',Prop)+1,Length(Prop));
                  Prop := Copy(Prop,Pos('"',Prop)+1,Length(Prop));

                  case IStrToInt(Prop) of
                  1:Font.Size := 8;
                  2:Font.Size := 10;
                  3:Font.Size := 12;
                  4:Font.Size := 14;
                  5:Font.Size := 16;
                  else
                    Font.Size := IStrToInt(Prop);
                  end;

                end;
              end;
            end;
        'S':begin
              TagChar := Upcase(s[3]);

              if TagChar = '>' then
                Font.Style := Font.Style + [fsStrikeOut]
              else
              begin
                if TagChar = 'H' then
                  isShad := True
                else
                begin
                  if ipos('<SUB>',s)=1 then
                    isSub := True
                  else
                    if ipos('<SUP>',s)=1 then
                      isSup := True;
                end;
              end;
            end;
        'Z':Invisible := True;
        end;
      end;

      if (VarPos('>',s,TagPos)>0) and not ImgBreak then
      begin
        Res := Res + Copy(s,1,TagPos);
        Delete(s,1,TagPos);
      end
      else
        if not Imgbreak then
          Delete(s,1,Length(s));
    end;
  end;

    w := w - sw;

    if w > xsize then
      xsize := w + 2;

    if (FocusLink = Hyperlinks - 1) and Anchor and not Calc then
    begin
      rr.Right := cr.Left;
      rr.Bottom := cr.Bottom;
      InflateRect(rr,1,0);
      ///if not Calc then
      ///  Canvas.DrawFocusRect(rr);
      rr.Left := r.Left + 1;
      rr.Top := rr.Bottom;
    end;

    Result := Res;
  end;

begin
  Anchor := False;
  Error := False;

  Font := TFont.Create;
  Font.Assign(AFont);
  
  Brush := TBrush.Create;
  OldFont := TFont.Create;
  OldFont.Assign(Font);

  DrawFont := TFont.Create;
  DrawFont.Assign(Font);

  CalcFont := TFont.Create;
  CalcFont.Assign(Font);

  OldDrawfont := TFont.Create;
  OldDrawFont.Assign(Font);

  OldCalcFont := TFont.Create;
  OldCalcFont.Assign(Font);
  
  BlnkColor := Brush.color;
  Brush.Color := clNone;
  BGColor := clNone;
  ParaColor := clNone;
  isPara := False;
  isShad := False;
  Invisible := False;

  OfsX := 0;
  NewOfsX := 0;

  Result := False;

  r := fr;
  r.Left := r.Left + 1; {required to add offset for DrawText problem with first capital W letter}

  Align := taLeftJustify;
  PIndent := 0;

  XSize := 0;
  YSize := 0;
  HyperLinks := 0;
  HlCount := 0;
  ListIndex := 0;
  LiCount := 0;
  StripVal := '';
  FocusAnchor := '';
  MouseLink := -1;
  MouseInAnchor := False;

  ImgIdx := 0;
  AltImg := -1;

  DrawStyle := DT_LEFT or DT_SINGLELINE or DT_EXTERNALLEADING or DT_BOTTOM  or DT_EXPANDTABS; // or DT_NOPREFIX;

  if Pos(' & ',s) > 0 then
    DrawStyle := DrawStyle or DT_NOPREFIX;


  if not WordWrap then
    DrawStyle := DrawStyle or DT_END_ELLIPSIS;

  if Pos('&',s) > 0 then
  begin
    repeat
      Foundtag := False;
      //if TagReplacestring('&lt;','<',s) then Foundtag := True;
      //if TagReplacestring('&gt;','>',s) then Foundtag := True;

      if TagReplacestring('&amp;','&&',s) then Foundtag := True;
      if TagReplacestring('&quot;','"',s) then Foundtag := True;

      if TagReplacestring('&sect;','§',s) then Foundtag := True;
      if TagReplacestring('&permil;','®‰',s) then Foundtag := True;
      if TagReplacestring('&reg;','®',s) then Foundtag := True;

      if TagReplacestring('&copy;','©',s) then Foundtag := True;
      if TagReplacestring('&para;','¶',s) then Foundtag := True;

      if TagReplacestring('&trade;','™',s) then Foundtag := True;
      if TagReplacestring('&euro;','€',s) then Foundtag := True;

    until not Foundtag;
  end;

  s := DBTagStrip(s);
  s := CRLFStrip(s,True);

  while Length(s) > 0 do
  begin
    {calculate part of the HTML text fitting on the next line}
    Oldfont.Assign(OldCalcFont);
    Font.Assign(CalcFont);
    Oldanchor := Anchor;
    OldAnchorVal := LastAnchor;
    suph := 0;
    subh := 0;
    imgali := 0;
    isSup := False;
    isSub := False;

    HtmlHeight := g.TextHeight('gh', font);
    txtHeight := HtmlHeight;

    OldImgIdx := ImgIdx;

    su := HTMLDrawLine(g,s,r,True,HtmlWidth,HtmlHeight,subh,suph,imgali,Align,PIndent,XPos,YPos,HotSpot,ImageHotSpot,ofsx,newofsx);

    Anchor := OldAnchor;
    LastAnchor := OldAnchorVal;

    CalcFont.Assign(Font);
    OldCalcFont.Assign(OldFont);

    if length(s) > 0 then
      HTMLHeight := HTMLHeight + LineSpacing;

    dr := r;

    case Align of
    taCenter:if (r.right - r.left - htmlwidth > 0) then
               dr.left := r.left+((r.right - r.left - htmlwidth) shr 1);
    taRightJustify:if r.right - htmlwidth > r.left then
                       dr.left := r.right - htmlwidth;
    end;

    dr.Left := dr.Left + PIndent;

    dr.Bottom := dr.Top + HtmlHeight + Subh + Suph;

    if not CheckHeight then
    begin
      OldFont.Assign(OldDrawFont);
      Font.Assign(DrawFont);

      HyperLinks := HlCount;
      ListIndex := LiCount;
      ImgIdx := OldImgIdx;

      HTMLDrawLine(g,su,dr,CheckHotSpot,HtmlWidth,HtmlHeight,subh,suph,ImgAli,Align,PIndent,XPos,YPos,HotSpot,ImageHotspot,ofsx,newofsx);

      HlCount := HyperLinks;
      LiCount := ListIndex;

      if (HotSpot and
         (YPos > dr.Bottom - ImgAli - g.TextHeight('gh', font)) and
         (YPos < dr.Bottom - ImgAli)) or ImageHotSpot then
      begin
        Result := True;
      end;

      ofsx := newofsx;

      DrawFont.Assign(Font);
      OldDrawFont.Assign(OldFont);
    end;

    r.top := r.top + HtmlHeight + subh + suph;
    ysize := ysize + HtmlHeight + subh + suph;

    {do not draw below bottom}
    if (r.top + TxtHeight > r.bottom) and not CheckHeight then
      s := '';
  end;

  if (ysize = 0) then
    ysize := g.TextHeight('gh', font);

  ysize := ysize + 2;

  Brush.Color := BlnkColor;
  Font.Assign(OldFont);
  OldFont.Free;
  DrawFont.Free;
  CalcFont.Free;                  
  OldDrawfont.Free;
  OldCalcfont.Free;
  Font.Free;
  Brush.Free;
end;
{$WARNINGS ON}


{$IFNDEF REMOVESTRIP}

function HTMLStrip(s:string):string;
var
  TagPos: integer;
begin
  Result := '';
  // replace line breaks by linefeeds
  while (pos('<BR>',uppercase(s))>0) do s := StringReplace(s,'<BR>',chr(13)+chr(10),[rfIgnoreCase]);
  while (pos('<HR>',uppercase(s))>0) do s := StringReplace(s,'<HR>',chr(13)+chr(10),[rfIgnoreCase]);

  while (VarPos('<z>',s,TagPos) > 0) do
  begin
    Result := Result + Copy(s,1,TagPos - 1); // copy till Z tag
    if (VarPos('</z>',s,TagPos) > 0) then
      Delete(s,1,TagPos + 3)
    else
      Break;
  end;

  while (VarPos('<Z>',s,TagPos) > 0) do
  begin
    Result := Result + Copy(s,1,TagPos - 1); // copy till Z tag
    if (VarPos('</Z>',s,TagPos) > 0) then
      Delete(s,1,TagPos + 3)
    else
      Break;
  end;


  // remove all other tags
  while (VarPos('<',s,TagPos) > 0) do
  begin
    Result := Result + Copy(s,1,TagPos - 1);
    if (VarPos('>',s,TagPos)>0) then
      Delete(s,1,TagPos)
    else
      Break;
  end;
  Result := Result + s;
end;
{$ENDIF}

{$IFDEF HILIGHT}

function HTMLStripAll(s:string):string;
var
  TagPos: integer;
begin
  Result := '';

  // remove all tags
  while (VarPos('<',s,TagPos)>0) do
  begin
    Result := Result + Copy(s,1,TagPos-1);
    if (VarPos('>',s,TagPos)>0) then
      Delete(s,1,TagPos);
  end;
  Result := Result + s;
end;

function StripPos2HTMLPos(s:string; i: Integer): Integer;
var
  j,k: Integer;
  Skip: Boolean;
begin
  Result := 0;
  k := 1;
  Skip := False;

  for j := 1 to Length(s) do
  begin
    if s[j] = '<' then
      Skip := True;

    if (k = i) and not Skip then
    begin
      Result := j;
      Exit;
    end;

    if not Skip then
      Inc(k);

    if s[j] = '>' then
      Skip := False;

  end;

  if k = i then
  begin
    Result := Length(s) + 1;
  end;
end;


function PosFrom(su,s:string; h: Integer;DoCase: boolean; var Res: Integer): Integer;
var
  r: Integer;
begin
  Result := 0;
  Res := 0;

  if h > 0 then
    Delete(s,1,h);

  if DoCase then
    r := Pos(su,s)
  else
    r := Pos(UpperCase(su),UpperCase(s));

  if r > 0 then
  begin
    Res := h + r;
    Result := Res;
  end;
end;

function HiLight(s,h,tag:string;DoCase:boolean):string;
var
  hs: string;
  l,k: Integer;
begin
  hs := HTMLStripAll(s);

  l := 0;

  while PosFrom(h,hs,l,DoCase,k) > 0 do
  begin
    l := k + Length(h);
    Insert('<'+tag+'>',s,StripPos2HTMLPos(s,k));
    Insert('</'+tag+'>',s,StripPos2HTMLPos(s,l));
  end;

  Result := s;
end;

function UnHiLight(s,tag:string):string;
begin
  Result := '';
  // replace line breaks by linefeeds
  tag := Uppercase(tag);
  while Pos('<'+tag+'>',Uppercase(s)) > 0 do s := StringReplace(s,'<'+tag+'>','',[rfIgnoreCase]);
  while Pos('</'+tag+'>',Uppercase(s)) > 0 do s := StringReplace(s,'</'+tag+'>','',[rfIgnoreCase]);
  Result := s;
end;

{$ENDIF}

{$IFDEF PARAMS}

function IPosv(su,s:string;var vp:integer):integer;
begin
  vp := pos(UpperCase(su),UpperCase(s));
  Result := vp;
end;


function GetHREFValue(html,href:string;var value:string):boolean;
var
  lp: Integer;
begin
  Result := False;
  while IPosv('href="',html,lp) > 0 do
  begin
    Delete(html,1,lp+5); {delete all before}
    if IPosv('"',html,lp) > 0 then
    begin
      if CompareText(href,copy(html,1,lp-1))=0 then
      begin
        {href match - get the value now}
        Delete(html,1,lp);
        if (iposv('>',html,lp)>0) then
        begin
          Delete(html,1,lp);
          if (iposv('<',html,lp)>0) then
          begin
            Value := Copy(html,1,lp-1);
            Result := True;
            Break;
          end;
        end;
      end;
    end;
  end;
end;


function SetHREFValue(var html:string;href,value:string):boolean;
var
  h:string;
  p:string;
begin
  {get current value and do a stringreplace}

  Result := False;
  if GetHREFValue(html,href,h) then
  begin
    p := Copy(html,pos('href="' + href,html),Length(html));

    p := StringReplace(p,'>' + h + '</A','>' + value + '</A',[rfIgnoreCase]);

    html := Copy(html,1,pos('href="'+href,html)-1)+p;
    Result := True;
  end;
end;

{$ENDIF}




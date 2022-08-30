{*************************************************************************}
{ Mini HTML rendering engine                                              }
{ for Delphi and C++Builder                                               }
{                                                                         }
{ written by TMS Software                                                 }
{            copyright © 1999-2012                                        }
{            Email : info@tmssoftware.com                                 }
{            Website : http://www.tmssoftware.com/                        }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

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
  //outputdebugstring(pchar(apath));

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
    try
      bmp.Width := rx;
      bmp.Height := ry;
      ImageList_Draw(imglsthandle,i,bmp.Canvas.handle,0,0,ILD_NORMAL);
      r.left := x;
      r.top := y;
      r.right := x + Round(rx * ResFactor);
      r.bottom := y + Round(ry * ResFactor);
      PrintBitmap(Canvas,r,bmp);
    finally
      bmp.Free;
    end;
  end;
end;


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

function Hex2Color(s:string):tcolor;
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
{
function IPosFrom(su,s:string;frm:integer):Integer;
var
  i:Integer;
begin
  i := Pos(UpperCase(su),UpperCase(s));
  if (i>frm) then
    Result := i
  else
    Result := 0;
end;

function CRLFStrip(s:string):string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
  begin
    if not (s[i] in [#13,#10]) then Result := Result + s[i];
  end;
end;

function iReplaceString(const srch,repl:string;var dest:string;var srchpos:integer):Boolean;
var
  i: Integer;
begin
  i := ipos(srch,dest);
  if (i > srchpos) then
  begin
    Result := True;
    Delete(dest,i,length(srch));
    dest := Copy(dest,1,i-1) + repl + Copy(dest,i,255);
    srchpos := i + length(repl);
  end
  else
    Result := False;
end;


}
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
  if (i>0) then
  begin
    Result := Copy(s,1,i - 1);
    Delete(s,1,i);
    j := Pos('>',s);
    if (j > 0) then
      Delete(s,j,1);
    Result := Result + s;
  end
  else
    Result := s;
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
function HTMLDrawEx(Canvas:tCanvas;s:string;fr:trect;
                                 fImages:TImageList;
                                 xpos,ypos,focuslink,hoverlink,shadowoffset:integer;
                                 checkhotspot,checkheight,print,selected,blink,hoverstyle,wordwrap:boolean;
                                 resfactor:double;
                                 urlcolor,hovercolor,hoverFontcolor,shadowcolor:tcolor;
                                 var anchorval,stripval,focusanchor:string;
                                 var xsize,ysize,hyperlinks,mouselink:integer;
                                 var hoverrect:trect):boolean;
var
  su:string;
  r,dr,hr,rr,er:trect;
  htmlwidth,htmlheight:integer;
  align:talignment;
  oldFont:TFont;
  calcFont:TFont;
  drawFont:TFont;
  oldcalcFont:TFont;
  olddrawFont:TFont;
  hotspot:boolean;
  anchor,oldanchor,mouseinanchor:boolean;
  bgcolor,paracolor,hvrcolor,hvrfntcolor,pencolor,blnkcolor:tcolor;
  lastanchor,oldanchorval:string;
  bmpsize:tpoint;
  isSup,isSub,isPara,isShad:boolean;
  subh,suph,imgali,srchpos,hlcount,licount:integer;
  hrgn,holdFont:thandle;
  listindex:integer;
  dtp:TDrawTextParams;
  DrawStyle: DWord;
  FoundTag: Boolean;
  invisible:boolean;
  {new for editing}
  nnFit:integer;
  nnSize:TSize;
  inspoint:integer;
  nndx:pointer;
  {end of new for editing}

  procedure StartRotated(Canvas: TCanvas; Angle: Integer);
  var
    LFont:TLogFont;
  begin
    GetObject(Canvas.Font.Handle,SizeOf(LFont),Addr(LFont));

    LFont.lfEscapement := Angle*10;
    LFont.lfOrientation := Angle*10;
    hOldFont:=SelectObject(Canvas.Handle,CreateFontIndirect(LFont));
  end;

  procedure EndRotated(Canvas:tCanvas);
  begin
    DeleteObject(SelectObject(Canvas.Handle,hOldFont));
  end;

  function HTMLDrawLine(Canvas:TCanvas;var s:string;r:TRect;Calc:Boolean;
                        var w,h,subh,suph,imgali:integer;var align:talignment;
                        xpos,ypos:integer;var hotspot:boolean):string;
  var
    su,res,tagprop,prop,tagp:string;
    cr: TRect;
    linebreak,imgbreak,linkbreak:boolean;
    th,sw,indent,err,bmpx,bmpy:integer;
    tagpos,spacepos:integer;
    bmp:tbitmap;
    newcolor:tcolor;
    TagWidth,TagHeight,WordLen,WordWidth,WordLenEx: Integer;
    TagChar: Char;
    LengthFits: Boolean;

  begin
    Result := '';
    r.Bottom := r.Bottom - Subh;
    w := 0;
    sw := 0;
    h := Canvas.TextHeight('gh');

    linebreak:=false;
    imgbreak:=false;
    linkbreak:=false;
    hotspot:=false;
    cr := r;
    res := '';

    if (isPara) and not calc then
    begin
      PenColor := Canvas.pen.color;
      Canvas.Pen.color := Canvas.brush.color;
      Canvas.Rectangle(fr.left,r.top,fr.right,r.top+h);
    end;

    while (Length(s) > 0) and not LineBreak and not ImgBreak do
    begin
      TagPos := Pos('<',s);
      SpacePos := Pos(' ',s);

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

      // next part of string until space

      WordLen := Length(su);

      repeat
        FoundTag := False;
        if TagReplacestring('&lt;','<',su) then Foundtag := True;
        if TagReplacestring('&gt;','>',su) then Foundtag := True;

        if TagReplacestring('&sect;','§',s) then Foundtag := True;
        if TagReplacestring('&permil;','®‰',s) then Foundtag := True;
        if TagReplacestring('&reg;','®',s) then Foundtag := True;

        if TagReplacestring('&copy;','©',s) then Foundtag := True;
        if TagReplacestring('&para;','¶',s) then Foundtag := True;

        if TagReplacestring('&trade;','™',s) then Foundtag := True;
        if TagReplacestring('&euro;','€',s) then Foundtag := True;

      until not FoundTag;

      WordLenEx := Length(su);
      WordWidth := 0;  


      if WordLenEx > 0 then
      begin
        th := Canvas.TextHeight(su);

        if isSub and (subh < (th shr 2)) then subh := th shr 2;
        if isSup and (suph < (th shr 2)) then suph := th shr 2;

        if th > h then h := th;

        Stripval := Stripval + su;

        if Invisible then
          Delete(s,1,WordLenEx);

        if not Invisible then
        begin
          // draw mode
          if not Calc then
          begin
            if isSup then cr.Bottom := cr.Bottom - suph;
            if isSub then cr.Bottom := cr.Bottom + subh;
            cr.Bottom := cr.Bottom - imgali;

            if isShad then
            begin
              OffsetRect(cr,ShadowOffset,ShadowOffset);
              NewColor := Canvas.Font.Color;
              Canvas.Font.Color := ShadowColor;
              DrawTextEx(Canvas.Handle,pchar(su),WordLen,cr,DrawStyle,nil);
              Offsetrect(cr,-ShadowOffset,-ShadowOffset);
              Canvas.Font.Color := NewColor;
            end;

            DrawTextEx(Canvas.Handle,PChar(su),WordLenEx,cr,DrawStyle,nil);
            DrawTextEx(Canvas.Handle,PChar(su),WordLenEx,cr,DrawStyle or DT_CALCRECT,nil);

            if Anchor and (Hyperlinks - 1 = FocusLink) then
              FocusAnchor := LastAnchor;

            {$IFDEF TMSDEBUG}
            if Anchor then
              DbgMsg('drawrect for '+anchorval+' = ['+inttostr(cr.Left)+':'+inttostr(cr.Top)+'] ['+inttostr(cr.right)+':'+inttostr(cr.bottom)+'] @ ['+inttostr(xpos)+':'+inttostr(ypos));
            {$ENDIF}

            cr.Left := cr.Right;
            cr.Right := r.Right;
            cr.Bottom := r.Bottom;
            cr.Top := r.Top;
          end
          else
          begin
            cr := r; //reinitialized each time
            DrawText(Canvas.Handle,PChar(su),WordLenEx,cr,DrawStyle or DT_CALCRECT);

            // modified for editing purposes
            if (ypos > cr.top) and (ypos < cr.bottom) and (xpos > w) then
            begin
              er := Rect(w,cr.top,xpos,cr.bottom);

              Fillchar(dtp,sizeof(dtp),0);
              dtp.cbSize := SizeOf(dtp);

              GetTextExtentExPoint(Canvas.handle,pchar(su),length(su),xpos-w,@nnfit,nil,nnSize);

              {this will get the character pos of the insertion point}
              if nnfit=length(su) then inspoint := inspoint+length(su) else
              begin
                inspoint:=inspoint+nnfit;
              end;
            end;
            {end of edit modify}

            {Calculated text width}
            WordWidth := cr.Right - cr.Left;
            w := w + WordWidth;

            if (xpos - cr.Left >= w - WordWidth) and (xpos - cr.Left <= w) and Anchor then
            begin
              Hotspot := True;
              if (ypos > cr.Top) and (ypos < cr.Bottom) then
              begin
                Anchorval := LastAnchor;
                MouseInAnchor := True;
              end;
            end;
          end;

          LengthFits := (w < r.Right - r.Left) or (r.Right - r.Left <= WordWidth);

          if LengthFits or not WordWrap then
          begin
            Res := Res + Copy(s,1,WordLen);
            if not LengthFits and Calc then s := '';

            Delete(s,1,WordLen);

            if su[WordLen] = ' ' then
              sw := Canvas.TextWidth(' ')
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

  if not LineBreak and (TagPos = 1) and (Length(s) > 2) then
   begin
     if (s[2] = '/') and (Length(s) > 3) then
       begin
        case Upcase(s[3]) of
        'A':begin
             if not hoverstyle or (hoverlink=hyperlinks) then
               begin
                Canvas.Font.style:=Canvas.Font.style-[fsUnderline];
                if hovercolor<>clNone then
                  begin
                   Canvas.brush.color:=hvrcolor;
                   if hvrcolor=clNone then Canvas.brush.style:=bsClear;
                  end;
                if hoverFontcolor<>clNone then
                  begin
                   Canvas.Font.color:=hoverFontcolor;
                  end;
               end;
             if not selected then Canvas.Font.color:=oldFont.color;
             anchor:=false;

             if mouseinanchor then
               begin
                hr.bottom:=r.bottom;
                hr.right:=r.left+w;
                if (r.top<>hr.top) then
                 begin
                  hr.left:=r.left;
                  hr.top:=r.top;
//                  hr.right:=r.right;
                 end;
                HoverRect:=hr;
                mouselink:=hyperlinks;
                {$IFDEF TMSDEBUG}
                DbgRect('hotspot anchor '+lastanchor,hr);
                {$ENDIF}
                mouseinanchor:=false;
               end;

             if (focuslink=hyperlinks-1) then
              begin
               rr.right:=cr.left;
               rr.bottom:=cr.bottom-imgali;
               rr.top:=rr.bottom-Canvas.textheight('gh');
               inflaterect(rr,1,0);
               if not calc then Canvas.drawfocusrect(rr);
              end;

            end;
        'B':begin
              if s[4] <> '>' then
                Canvas.Font.Color := OldFont.Color
              else
                Canvas.Font.Style := Canvas.Font.Style - [fsBold];
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
                Canvas.Font.Style := Canvas.Font.Style - [fsStrikeOut];
            end;
        'F':begin
             Canvas.Font.name:=oldFont.name;
             Canvas.Font.size:=oldFont.size;
             if not calc and not selected then
              begin
                Canvas.Font.color:=oldFont.color;
                Canvas.brush.color:=bgcolor;
                if bgcolor=clNone then
                  begin
                   Canvas.brush.style:=bsClear;
                  end;
              end;
            end;
        'I':begin
             Canvas.Font.style:=Canvas.Font.style-[fsItalic];
            end;
        'P':begin
             linebreak:=true;
             if not calc then
              begin
               Canvas.brush.color:=paracolor;
               if paracolor=clNone then Canvas.brush.style:=bsClear;
               isPara:=false;
              end;
            end;
        'U':begin
              if (s[4] <> '>') and (ListIndex > 0) then
                Dec(Listindex)
              else
                Canvas.Font.Style := Canvas.Font.Style - [fsUnderline];
            end;
        'R':begin
              EndRotated(Canvas);
            end;
        'Z':Invisible:=false;
        end;
       end
     else
       begin
        case upcase(s[2]) of
        'A':begin
             {only do this when at hover position in xpos,ypos}
             if (focuslink=hyperlinks) and not calc then
               begin
                rr.left:=cr.left;
                rr.top:=cr.top;
               end;

             inc(hyperlinks);
             if not hoverstyle or (hoverlink=hyperlinks) then
               begin
                 Canvas.Font.style:=Canvas.Font.style+[fsUnderline];
                 if hovercolor<>clNone then
                  begin
                   hvrcolor:=Canvas.brush.color;
                   if Canvas.brush.style=bsClear then hvrcolor:=clNone;
                   Canvas.brush.color:=HoverColor;
                  end;
                 if hoverFontcolor<>clNone then
                  begin
                   hvrfntcolor:=Canvas.Font.color;
                   Canvas.Font.color:=hoverFontcolor;
                  end;
               end;


             if not selected and ((hoverFontcolor=clNone) or (hoverlink<>hyperlinks) or not hoverstyle) then Canvas.Font.color:=urlcolor;

             tagprop:=copy(s,3,pos('>',s)-1);
             prop:=copy(tagprop,ipos('href',tagprop)+4,length(tagprop));
             prop:=copy(prop,pos('"',prop)+1,length(prop));
             prop:=copy(prop,1,pos('"',prop)-1);
             lastanchor:=prop;
             anchor:=true;

             hr.left:=w;
             hr.top:=r.top;

            end;
        'B':begin
              TagChar := Upcase(s[3]);
              if TagChar = '>' then  // <B> tag
                Canvas.Font.Style := Canvas.Font.Style + [fsBold]
              else
                if TagChar = 'R' then // <BR> tag
                begin
                  LineBreak := true;
                  StripVal := StripVal + #13;
                end
                else
                begin
                  if TagChar = 'L' then // <BLINK> tag
                  begin
                    if not blink then Canvas.Font.Color := BlnkColor;
                  end
                  else
                  if TagChar = 'O' then  // <BODY ... >
                   begin
                    res:=res+copy(s,1,pos('>',s));
                    tagprop:=copy(s,6,pos('>',s)-1);

                    if (ipos('background',tagprop)>0) and not calc then
                     begin

                      prop:=copy(tagprop,ipos('background',tagprop)+10,length(tagprop));
                      prop:=copy(prop,pos('"',prop)+1,length(prop));
                      prop:=copy(prop,1,pos('"',prop)-1);
                      if ipos('file://',prop)>0 then
                       begin
                        delete(prop,1,7);

                        if FileExists(prop) then
                         begin
                          bmp:=tbitmap.Create;
                          try
                          bmp.LoadFromFile(prop);
                          {do the tiling here}
                          bmpy:=0;

                          hrgn := CreateRectRgn(fr.left, fr.top, fr.right,fr.bottom);
                          SelectClipRgn(Canvas.handle, hrgn);

                          while (bmpy < fr.bottom-fr.top) do
                           begin
                            bmpx:=0;
                            while (bmpx < fr.right-fr.left) do
                             begin
                              Canvas.draw(fr.left+bmpx,fr.top+bmpy,bmp);
                              bmpx:=bmpx+bmp.width;
                            end;
                            bmpy:=bmpy+bmp.height;
                           end;

                          finally
                          bmp.Free;
                          SelectClipRgn(Canvas.handle, 0);
                          end;
                         end;
                       end;

                     end;

                    if (ipos('bgcolor',tagprop)>0) then
                     begin
                      prop:=copy(tagprop,ipos('bgcolor',tagprop)+7,length(tagprop));

                      prop:=copy(prop,pos('"',prop)+1,length(prop));
                      prop:=copy(prop,1,pos('"',prop)-1);
                      if not calc then
                      begin
                       if (pos('cl',prop)>0) then
                         Canvas.brush.color:=text2color(ansilowercase(prop));
                       if (pos('#',prop)>0) then
                         Canvas.brush.color:=hex2color(prop);

                      if not calc then
                       begin
                        bgcolor:=Canvas.brush.color;
                        pencolor:=Canvas.pen.color;
                        Canvas.pen.color:=bgcolor;
                        Canvas.rectangle(fr.left,fr.top,fr.right,fr.bottom);
                        Canvas.pen.color:=pencolor;
                       end;
                      end;
                     end;
                   end;
                end;
            end;
        'H':begin
             linebreak:=true;
             if not calc then
              begin
               pencolor:=Canvas.pen.color;
               Canvas.pen.color:=clblack;
               Canvas.MoveTo(r.left,cr.bottom+1);
               Canvas.Lineto(r.right,cr.bottom+1);
               Canvas.pen.color:=pencolor;
              end;
            end;
        'I':begin
             if ipos('<I>',s)=1 then
               begin
                Canvas.Font.style:=Canvas.Font.style+[fsItalic];
               end;
             if ipos('<IND',s)=1 then
               begin
                tagprop:=copy(s,3,pos('>',s)-1);
                prop:=copy(tagprop,ipos('x',tagprop)+2,length(tagprop));
                prop:=copy(prop,pos('"',prop)+1,length(prop));
                prop:=copy(prop,1,pos('"',prop)-1);

                val(prop,indent,err);
                if (err=0) then
                 begin
                  if indent>w then
                    begin
                     w:=indent;
                     cr.left:=fr.left+indent;
                    end;
                 end;
               end;

             if ipos('<IMG',s)=1 then
               begin
                //oldFont.color:=Canvas.Font.color;
                tagprop:=copy(s,3,pos('>',s)-1);
                prop:=copy(tagprop,ipos('src',tagprop)+4,length(tagprop));
                prop:=copy(prop,pos('"',prop)+1,length(prop));
                prop:=copy(prop,1,pos('"',prop)-1);

                tagwidth:=0; tagheight:=0;

                if (pos('width',tagprop)>0) then
                 begin
                   tagp:=copy(tagprop,ipos('width',tagprop)+6,length(tagprop));
                   tagp:=copy(tagp,pos('"',tagp)+1,length(tagp));
                   tagp:=copy(tagp,1,pos('"',tagp)-1);
                   val(tagp,tagwidth,err);
                 end;

                if (pos('height',tagprop)>0) then
                 begin
                   tagp:=copy(tagprop,ipos('height',tagprop)+7,length(tagprop));
                   tagp:=copy(tagp,pos('"',tagp)+1,length(tagp));
                   tagp:=copy(tagp,1,pos('"',tagp)-1);
                   val(tagp,tagheight,err);
                 end;

                bmpsize.x:=0;
                bmpsize.y:=0;

                if ipos('idx:',prop)>0 then
                  begin
                   delete(prop,1,4);
                   if assigned(fImages) and (istrtoint(prop)<fImages.Count) then
                    begin
                      bmpsize.x:=fImages.Width;
                      bmpsize.y:=fImages.Height;

                      if (not calc) and not print then
                       fImages.Draw(Canvas,cr.left,cr.top,istrtoint(prop),true);

                      if (not calc) and print then
                       begin
                        cr.right:=cr.left+round(resfactor*bmpsize.x);
                        cr.bottom:=cr.top+round(resfactor*bmpsize.y);

                        bmp:=tbitmap.Create;
                        fImages.GetBitmap(istrtoint(prop),bmp);
                        PrintBitmap(Canvas,cr,bmp);
                        bmp.free;
                        cr := r;
                       end;
                    end;
                  end;

                if ipos('ssys:',prop)>0 then
                  begin
                   delete(prop,1,5);
                   bmpsize:=sysimage(Canvas,cr.left,cr.top,prop,false,not calc,print,resfactor);
                  end;

                if ipos('lsys:',prop)>0 then
                  begin
                   delete(prop,1,5);
                   bmpsize:=sysimage(Canvas,cr.left,cr.top,prop,true,not calc,print,resfactor);
                  end;

                if ipos('res://',prop)>0 then
                  begin
                    delete(prop,1,6);
                    if findresource(hinstance,pchar(prop),RT_BITMAP)<>0 then
                    begin
                    bmp:=tbitmap.Create;
                    try
                     bmp.LoadFromResourceName(hinstance,prop);
                     if not calc and not print then Canvas.Draw(cr.left,cr.top,bmp);
                     if not calc and print then
                       begin
                        cr.right:=cr.left+round(resfactor*bmpsize.x);
                        cr.bottom:=cr.top+round(resfactor*bmpsize.y);
                        PrintBitmap(Canvas,cr,bmp);
                       end;
                      bmpsize.x:=bmp.width;
                      bmpsize.y:=bmp.height;
                    finally
                      bmp.Free;
                    end;
                    end;
                  end;

                if ipos('file://',prop)>0 then
                  begin
                    delete(prop,1,7);
                    if FileExists(prop) then
                     begin
                      bmp:=tbitmap.Create;
                      try
                      bmp.LoadFromFile(prop);
                      if not calc and not print then
                        begin
                         if (tagwidth>0) and (tagheight>0) then
                           begin
                             Canvas.StretchDraw(rect(cr.left,cr.top,cr.left+tagwidth,cr.top+tagheight),bmp);

                           end
                         else
                           Canvas.Draw(cr.left,cr.top,bmp);
                        end;

                      if (tagwidth>0) and (tagheight>0) then
                       begin
                         bmpsize.x:=tagwidth;
                         bmpsize.y:=tagheight;
                       end
                      else
                       begin
                         bmpsize.x:=bmp.width;
                         bmpsize.y:=bmp.height;
                       end;

                      if not calc and print then
                       begin
                        cr.right:=cr.left+round(resfactor*bmpsize.x);
                        cr.bottom:=cr.top+round(resfactor*bmpsize.y);
                        PrintBitmap(Canvas,cr,bmp);
                       end;

                      finally
                      bmp.Free;
                      end;
                     end;
                  end;

                 if (xpos-r.left>w) and (xpos-r.left<w+bmpsize.x) and
                   (ypos>cr.top) and (ypos<cr.bottom) and Anchor then
                 begin
                   hotspot := true;
                   anchorval := lastanchor;
                 end;

                 if print then
                 begin
                   bmpsize.x:=round(bmpsize.x*resfactor);
                   bmpsize.y:=round(bmpsize.y*resfactor);
                   {$IFDEF TMSDEBUG}
                   DbgPoint('bmp : ',point(bmpsize.x,bmpsize.y));
                   {$ENDIF}
                 end;

                 if (w+bmpsize.x>r.right-r.left) and (bmpsize.x<r.right-r.left) then
                 begin
                   imgbreak:=true;
                 end
                 else
                   begin
                    w:=w+bmpsize.x;
                    cr.left:=cr.left+bmpsize.x;
                    if (bmpsize.y>h) then h:=bmpsize.y;
                   end;

                 if (ipos('align',tagprop)>0) then
                  begin
                   if (ipos('"top',tagprop)>0) then
                    begin
                      imgali:=h-Canvas.textheight('gh');
                    end
                   else
                    begin
                     if (ipos('"middle',tagprop)>0) then
                      begin
                        imgali:=(h-Canvas.textheight('gh')) shr 1;
                      end;
                    end;
                  end;
               end;
            end;
        'L':begin
             w:=w+12*listindex;
             if linkbreak then imgbreak:=true else linkbreak:=true;
             cr.left:=cr.left+12*(listindex-1);
             if not calc then
              begin
               prop:=Canvas.Font.name;
               Canvas.Font.name:='Symbol';
               if odd(listindex) then
               drawtext(Canvas.handle,'·',1,cr,0)
               else
               drawtext(Canvas.handle,'o',1,cr,0);
               Canvas.Font.name:=prop;
              end;
             cr.left:=cr.left+12;
            end;
        'U':begin
              if s[3] <> '>' then
              begin
                Inc(ListIndex);
              end
              else
                Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];
            end;
        'P':begin
             if (pos('>',s)>0) then
              begin
               tagprop:=copy(s,3,pos('>',s)-1);

               if (ipos('align',tagprop)>0) then
                begin
                 prop:=copy(tagprop,ipos('align',tagprop)+5,length(tagprop));
                 prop:=copy(prop,pos('"',prop)+1,length(prop));
                 prop:=copy(prop,1,pos('"',prop)-1);

                 if (ipos('right',prop)>0) then align:=taRightJustify;
                 if (ipos('left',prop)>0) then align:=taLeftJustify;
                 if (ipos('center',prop)>0) then align:=taCenter;
                end;

               if (ipos('bgcolor',tagprop)>0) then
                begin
                 prop:=copy(tagprop,ipos('bgcolor',tagprop)+5,length(tagprop));
                 prop:=copy(prop,pos('"',prop)+1,length(prop));
                 prop:=copy(prop,1,pos('"',prop)-1);

                 newcolor:=clNone;
                 if (pos('cl',prop)>0) then
                  begin
                   newcolor:=text2color(ansilowercase(prop));
                  end;
                 if (pos('#',prop)>0) then
                  begin
                   newcolor:=hex2color(prop);
                  end;

                 if not calc then
                  begin
                   isPara:=true;
                   paracolor:=Canvas.brush.color;
                   if Canvas.brush.style=bsClear then paracolor:=clNone;
                   Canvas.brush.color:=newcolor;
                   pencolor:=Canvas.pen.color;
                   Canvas.pen.color:=newcolor;
                   Canvas.rectangle(fr.left,r.top,fr.right,r.bottom);
                  end;
                end;

              end;
            end;
        'F':begin
            if (pos('>',s)>0) then
             begin
              tagprop:=copy(s,5,pos('>',s)-1);
              if (ipos('face',tagprop)>0) then
                begin
                 prop:=copy(tagprop,ipos('face',tagprop)+4,length(tagprop));
                 prop:=copy(prop,pos('"',prop)+1,length(prop));
                 prop:=copy(prop,1,pos('"',prop)-1);
                 //oldFont.name:=Canvas.Font.name;
                 Canvas.Font.name:=prop;
                end;
              if (ipos(' color',tagprop)>0) and (selected=false) then
                begin
                 prop:=copy(tagprop,ipos('color',tagprop)+4,length(tagprop));
                 prop:=copy(prop,pos('"',prop)+1,length(prop));
                 prop:=copy(prop,1,pos('"',prop)-1);
                 //oldFont.color:=Canvas.Font.color;

                 if (pos('cl',prop)>0) then
                  begin
                   Canvas.Font.color:=text2color(ansilowercase(prop));
                  end;
                 if (pos('#',prop)>0) then
                  begin
                   Canvas.Font.color:=hex2color(prop);
                  end;
                end;
              if (ipos('bgcolor',tagprop)>0) and not calc and not selected then
                begin
                 prop:=copy(tagprop,ipos('bgcolor',tagprop)+4,length(tagprop));
                 prop:=copy(prop,pos('"',prop)+1,length(prop));
                 prop:=copy(prop,1,pos('"',prop)-1);

                 bgcolor:=Canvas.brush.color;
                 if Canvas.brush.style=bsClear then bgcolor:=clNone;

                 if (pos('cl',prop)>0) then
                  begin
                   Canvas.brush.color:=text2color(ansilowercase(prop));
                  end;
                 if (pos('#',prop)>0) then
                  begin
                   Canvas.brush.color:=hex2color(prop);
                  end;
                end;

              if (ipos('size',tagprop)>0) then
                begin
                 prop:=copy(tagprop,ipos('size',tagprop)+4,length(tagprop));
                 prop:=copy(prop,pos('"',prop)+1,length(prop));
                 prop:=copy(prop,1,pos('"',prop)-1);
                 //oldFont.size:=Canvas.Font.size;
                 Canvas.Font.size:=istrtoint(prop);
                end;
             end;

            end;
        'S':begin
              if (ipos('<S>',s)=1) then
               begin
                Canvas.Font.style:=Canvas.Font.style+[fsStrikeOut];
               end;
              if (ipos('<SUB>',s)=1) then
               begin
                isSub:=true;
               end;
              if (ipos('<SUP>',s)=1) then
               begin
                isSup:=true;
               end;
              if (ipos('<SHAD>',s)=1) then
               begin
                isShad:=true;
               end;
            end;
        'R':begin
              tagprop:=copy(s,3,pos('>',s)-1);
              prop:=copy(tagprop,ipos('a',tagprop)+2,length(tagprop));
              prop:=copy(prop,pos('"',prop)+1,length(prop));
              prop:=copy(prop,1,pos('"',prop)-1);
              val(prop,indent,err);
              StartRotated(Canvas,indent);
            end;
        'Z':Invisible:=true;
         end;
       end;

      if (VarPos('>',s,TagPos) > 0) and not ImgBreak then
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
    xsize := w;

  if (FocusLink = HyperLinks - 1) and Anchor and not Calc then
  begin
    rr.right:=cr.left;
    rr.bottom:=cr.bottom;
    InflateRect(rr,1,0);
    if not Calc then Canvas.DrawFocusRect(rr);
    rr.left := r.left + 1;
    rr.top := rr.bottom;
  end;

  Result := Res;
  end;

begin
  anchor:=false;
  oldFont:=tFont.Create;
  oldFont.Assign(Canvas.Font);
  drawFont:=tFont.Create;
  drawFont.Assign(Canvas.Font);
  calcFont:=tFont.Create;
  calcFont.Assign(Canvas.Font);
  olddrawFont:=tFont.Create;
  olddrawFont.Assign(Canvas.Font);
  oldcalcFont:=tFont.Create;
  oldcalcFont.Assign(Canvas.Font);
  blnkcolor:=Canvas.brush.color;
  Canvas.brush.color:=clNone;
  bgcolor:=clNone;
  paracolor:=clNone;
  isPara:=false; isShad:=false;

  Result := false;
  r:=fr;
  r.left:=r.left+1; {required to add offset for DrawText problem with first capital W letter}

  align:=taLeftJustify;

  xsize:=0; ysize:=0; hyperlinks:=0; hlcount:=0; listindex:=0; licount:=0;
  stripval:='';
  focusanchor:='';
  mouselink:=-1;
  mouseinanchor:=false;
  Invisible:=false;

  SetBKMode(Canvas.handle,TRANSPARENT);

  DrawStyle := DT_LEFT or DT_SINGLELINE or DT_EXTERNALLEADING or DT_BOTTOM  or DT_EXPANDTABS or DT_NOPREFIX or DT_NOCLIP;

  if not WordWrap then
    DrawStyle := DrawStyle or DT_END_ELLIPSIS;

  if Pos('&',s) > 0 then
  begin
    repeat
      Foundtag := False;
      if TagReplacestring('&amp;','&',s) then Foundtag := True;
      if TagReplacestring('&quot;','"',s) then Foundtag := True;
    until not Foundtag;
  end;

  s := DBTagStrip(s);

  Inspoint:=0;

  while Length(s)>0 do
  begin
    oldFont.Assign(oldcalcFont);
    Canvas.Font.Assign(calcFont);
    oldanchor:=anchor;
    oldanchorval:=lastanchor;
    suph:=0; subh:=0; imgali:=0;
    isSup:=false; isSub:=false;

    su:=HTMLDrawLine(Canvas,s,r,true,htmlwidth,htmlheight,subh,suph,imgali,align,xpos,ypos,hotspot);

    anchor:=oldanchor;
    lastanchor:=oldanchorval;

    CalcFont.Assign(Canvas.Font);
    OldCalcFont.Assign(oldFont);

    dr:=r;
    case align of
    taCenter:dr.left:=r.left+((r.right-r.left-htmlwidth) shr 1);
    taRightJustify:dr.left:=r.right-htmlwidth;
    end;

    dr.bottom:= dr.top+htmlheight+subh+suph;

    if not checkheight then
    begin
      oldFont.Assign(olddrawFont);
      Canvas.Font.Assign(drawFont);

      hyperlinks:=hlcount;
      listindex:=licount;

      HTMLDrawLine(Canvas,su,dr,checkhotspot,htmlwidth,htmlheight,subh,suph,imgali,align,xpos,ypos,hotspot);

      hlcount:=hyperlinks;
      licount:=listindex;

      if hotspot and (ypos>dr.bottom-imgali-Canvas.textheight('gh')) and (ypos<dr.bottom-imgali) then
        Result := true;

      drawFont.Assign(Canvas.Font);
      olddrawFont.Assign(oldFont);
    end;

    r.top:=r.top+htmlheight+subh+suph;

    ysize:=ysize+htmlheight+subh+suph;

    if (r.top+htmlheight>r.bottom) and not checkheight then s:='';
  end;

  if (ysize=0) then
    ysize := Canvas.textheight('gh');
  inspoint := inspoint shr 1;

  Canvas.brush.color:=blnkcolor;
  Canvas.Font.assign(oldFont);
  oldFont.Free;
  drawFont.Free;
  calcFont.Free;
  olddrawFont.Free;
  oldcalcFont.Free;
end;
{$WARNINGS ON}

{$IFNDEF REMOVEDRAW}
function HTMLDraw(Canvas:tCanvas;s:string;fr:trect;
                                 FImages:TImageList;
                                 xpos,ypos:integer;
                                 Checkhotspot,Checkheight,Print,Selected,Blink,WordWrap:boolean;
                                 Resfactor:Double;
                                 urlcolor:tcolor;
                                 var anchorval,stripval:string;
                                 var xsize,ysize:integer):boolean;
var
 hyperlinks,mouselink:integer;
 focusanchor:string;
 r:trect;
begin
 Result := HTMLDrawEx(Canvas,s,fr,fImages,xpos,ypos,-1,-1,1,Checkhotspot,Checkheight,Print,Selected,Blink,False,WordWrap,
                    resfactor,urlcolor,clNone,clNone,clGray,anchorval,stripval,focusanchor,xsize,ysize,hyperlinks,mouselink,r);
end;
{$ENDIF}


{$IFNDEF REMOVESTRIP}
function HTMLStrip(s:string):string;
var
  Res: string;
  i: Integer;
begin
  res:='';
  {replace line breaks by linefeeds}
  while (pos('<BR>',uppercase(s))>0) do s:=StringReplace(s,'<BR>',chr(13)+chr(10),[rfIgnoreCase]);
  while (pos('<HR>',uppercase(s))>0) do s:=StringReplace(s,'<HR>',chr(13)+chr(10),[rfIgnoreCase]);

  {remove all other tags}
  while (pos('<',s)>0) do
  begin
    i:=pos('<',s);
    res:=res+copy(s,1,i-1);
    if (pos('>',s)>0) then
      Delete(s,1,pos('>',s));
  end;

  Result := res + s;
end;
{$ENDIF}

{$IFDEF PARAMS}
function IPosv(su,s:string;var vp:integer):integer;
begin
  Result := Pos(uppercase(su),uppercase(s));
  vp := Result;
end;

function GetHREFValue(html,href:string;var value:string):boolean;
var
  lp: Integer;
begin
  Result := False;
  while IPosv('href="',html,lp) > 0 do
  begin
    Delete(html,1,lp+5); //delete all before
    if (iposv('"',html,lp)>0) then
    begin
      if CompareText(href,copy(html,1,lp-1))=0 then
      begin
        // href match - get the value now
        Delete(html,1,lp);
        if (iposv('>',html,lp)>0) then
        begin
          Delete(html,1,lp);
          if (iposv('<',html,lp)>0) then
          begin
            value := copy(html,1,lp-1);
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
  Result := false;
  if GetHREFValue(html,href,h) then
  begin
    p:=copy(html,pos('href="'+href,html),length(html));
    p:=StringReplace(p,'>'+h+'</A','>'+value+'</A',[rfIgnoreCase]);
    html:=copy(html,1,pos('href="'+href,html)-1)+p;
    Result := true;
  end;
end;
{$ENDIF}




{********************************************************************
Mini HTML rendering engine
for Delphi 2.0,3.0,4.0,5.0 + C++Builder 1.0,3.0,4.0,5.0
version 1.6 - rel. June, 2000

written by TMS Software
           Copyright © 1999-2000
           Email : info@tmssoftware.com
           Website : http://www.tmssoftware.com/

The source code is given as is. The author is not responsible
for any possible damage done due to the use of this code.
The component can be freely used in any application. The complete
source code remains property of the author and may not be distributed,
published, given or sold in any form as such. No parts of the source
code can be included in any other component or application without
written authorization of the author.
********************************************************************}

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

function SysImage(canvas:tcanvas;x,y:integer;apath:string;large,draw,print:boolean;resfactor:double):tpoint;
var
  SFI: TSHFileInfo;
  i,err:integer;
  imglsthandle:thandle;
  rx,ry:integer;
  bmp:TBitmap;
  r:trect;
begin
  val(apath,i,err);

  if (apath<>'') and (err<>0) then
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
         SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES {or OPEN_FLAG[Open] or
         SELECTED_FLAG[Selected]});
   i := SFI.iIcon;
  end;

  if Large then
    imglsthandle := SHGetFileInfo('', 0, SFI, SizeOf(SFI),
                            SHGFI_SYSICONINDEX or SHGFI_LARGEICON)
  else
    imglsthandle := SHGetFileInfo('', 0, SFI, SizeOf(SFI),
                            SHGFI_SYSICONINDEX or SHGFI_SMALLICON);

  ImageList_GetIconSize(imglsthandle,rx,ry);
  result:=point(rx,ry);

  if draw and not print then
   ImageList_Draw(imglsthandle,i,canvas.handle,x,y, ILD_TRANSPARENT);

  if draw and print then
   begin
     bmp:=TBitmap.Create;
     bmp.width:=rx;
     bmp.height:=ry;
     ImageList_Draw(imglsthandle,i,bmp.canvas.handle,0,0,ILD_NORMAL);
     r.left:=x;
     r.top:=y;
     r.right:=x+round(rx*resfactor);
     r.bottom:=y+round(ry*resfactor);
     PrintBitmap(canvas,r,bmp);
     bmp.Free;
   end;
end;

function Text2Color(s:string):tcolor;
begin
  result:=clBlack;
  if (s='clred') then result:=clred;
  if (s='clblack') then result:=clblack;
  if (s='clblue') then result:=clblue;
  if (s='clgreen') then result:=clgreen;
  if (s='claqua') then result:=claqua;
  if (s='clyellow') then result:=clyellow;
  if (s='clfuchsia') then result:=clfuchsia;
  if (s='clwhite') then result:=clwhite;
  if (s='cllime') then result:=cllime;
  if (s='clsilver') then result:=clsilver;
  if (s='clgray') then result:=clgray;
  if (s='clolive') then result:=clolive;
  if (s='clnavy') then result:=clnavy;
  if (s='clpurple') then result:=clpurple;
  if (s='clteal') then result:=clteal;
  if (s='clmaroon') then result:=clmaroon;

  if (s='clbackground') then result:=clbackground;
  if (s='clactivecaption') then result:=clactivecaption;
  if (s='clinactivecaption') then result:=clinactivecaption;
  if (s='clmenu') then result:=clmenu;
  if (s='clwindow') then result:=clwindow;
  if (s='clwindowframe') then result:=clwindowframe;
  if (s='clmenutext') then result:=clmenutext;
  if (s='clwindowtext') then result:=clwindowtext;
  if (s='clcaptiontext') then result:=clcaptiontext;
  if (s='clactiveborder') then result:=clactiveborder;
  if (s='clinactiveborder') then result:=clinactiveborder;
  if (s='clappworkspace') then result:=clappworkspace;
  if (s='clhighlight') then result:=clhighlight;
  if (s='clhighlighttext') then result:=clhighlighttext;
  if (s='clbtnface') then result:=clbtnface;
  if (s='clbtnshadow') then result:=clbtnshadow;
  if (s='clgraytext') then result:=clgraytext;
  if (s='clbtntext') then result:=clbtntext;
  if (s='clinactivecaptiontext') then result:=clinactivecaptiontext;
  if (s='clbtnhighlight') then result:=clbtnhighlight;
  if (s='cl3ddkshadow') then result:=clgraytext;
  if (s='cl3dlight') then result:=cl3dlight;
  if (s='clinfotext') then result:=clinfotext;
  if (s='clinfobk') then result:=clinfobk;
end;

function HexVal(s:string):integer;
var
  i,j:integer;
begin
  i:=ord(s[1])-ord('0');
  if (i>10) then i:=10+i-(ord('A')-ord('0'));
  j:=ord(s[2])-ord('0');
  if (j>10) then j:=10+j-(ord('A')-ord('0'));
  result:=i*16+j;
end;

function Hex2Color(s:string):tcolor;
var
  r,g,b:integer;
begin
  delete(s,1,1);
  r:=hexval(uppercase(copy(s,1,2)));
  g:=hexval(uppercase(copy(s,3,2)));
  b:=hexval(uppercase(copy(s,5,2)));
  result:=tcolor(b*256*256+g*256+r);
end;

function ipos(su,s:string):integer;
begin
  result:=pos(uppercase(su),uppercase(s));
end;

function iposfrom(su,s:string;frm:integer):integer;
var
  i:integer;
begin
  i:=pos(uppercase(su),uppercase(s));
  if (i>frm) then result:=i else result:=0;
end;

function istrtoint(s:string):integer;
var
  err,res:integer;
begin
  val(s,res,err);
  result:=res;
end;

{
function ReplaceString(const srch,repl:string;var dest:string;var srchpos:integer):boolean;
var
  i:integer;
begin
  i:=pos(srch,dest);
  if i>srchpos then
   begin
    result:=true;
    delete(dest,i,length(srch));
    dest:=copy(dest,1,i)+repl+copy(dest,i+1,255);
    srchpos:=i+length(repl);
   end
  else result:=false;
end;
}

function dbtagstrip(s:string):string;
var
 i,j:integer;
begin
 i:=pos('<#',s);
 if (i>0) then
  begin
    result:=copy(s,1,i-1);
    delete(s,1,i);
    j:=pos('>',s);
    if (j>0) then delete(s,pos('>',s),1);
    result:=result+s;
  end
 else
  result:=s;
end;

function iReplaceString(const srch,repl:string;var dest:string;var srchpos:integer):boolean;
var
  i:integer;
begin
  i:=ipos(srch,dest);
  if i>srchpos then
   begin
    result:=true;
    delete(dest,i,length(srch));
    dest:=copy(dest,1,i-1)+repl+copy(dest,i,255);
    srchpos:=i+length(repl);
   end
  else result:=false;
end;


function HTMLDrawEx(canvas:TCanvas;s:string;fr:TRect;
                                 fImages:TImageList;
                                 xpos,ypos,focuslink,hoverlink,shadowoffset:integer;
                                 checkhotspot,checkheight,print,selected,blink,hoverstyle:boolean;
                                 resfactor:double;
                                 urlcolor,hovercolor,hoverfontcolor,shadowcolor:tcolor;
                                 var anchorval,stripval,focusanchor:string;
                                 var xsize,ysize,hyperlinks,mouselink:integer;
                                 var hoverrect:trect;fFields:{$IFDEF DBAWARE}TFields{$ELSE}TObject{$ENDIF}):boolean;
var
 su:string;
 r,dr,hr,rr,er:trect;
 htmlwidth,htmlheight:integer;
 align:talignment;
 oldfont:tfont;
 calcfont:tfont;
 drawfont:tfont;
 oldcalcfont:tfont;
 olddrawfont:tfont;
 hotspot:boolean;
 anchor,oldanchor,mouseinanchor:boolean;
 bgcolor,paracolor,hvrcolor,hvrfntcolor,pencolor,blnkcolor:tcolor;
 lastanchor,oldanchorval:string;
 bmpsize:tpoint;
 isSup,isSub,isPara,isShad:boolean;
 subh,suph,imgali,srchpos,hlcount,licount:integer;
 hrgn,holdfont:thandle;
 listindex:integer;
 dtp:TDrawTextParams;
 {new for editing}
 nnFit:integer;
 nnSize:TSize;
 inspoint:integer;
 nndx:pointer; 
 {end of new for editing}

  procedure StartRotated(canvas:tcanvas;angle:integer);
  var
    LFont:TLogFont;
  begin
    GetObject(canvas.Font.Handle,SizeOf(LFont),Addr(LFont));
    LFont.lfEscapement:=angle*10;
    LFont.lfOrientation:=angle*10;
    hOldFont:=SelectObject(canvas.Handle,CreateFontIndirect(LFont));
  end;

  procedure EndRotated(canvas:tcanvas);
  begin
    DeleteObject(SelectObject(canvas.Handle,hOldFont));
  end;

 {$WARNINGS OFF}
 function HTMLDrawLine(canvas:tcanvas;var s:string;r:trect;calc:boolean;
                       var w,h,subh,suph,imgali:integer;var align:talignment;
                       xpos,ypos:integer;var hotspot:boolean):string;
 var
  su,dsu,res,tagprop,prop,tagp:string;
  cr:trect;
  linebreak,imgbreak,linkbreak:boolean;
  th,sw,indent,err,bmpx,bmpy:integer;
  tagpos,spacepos:integer;
  bmp:tbitmap;
  newcolor:tcolor;
  tagwidth,tagheight:integer;
  tagtransp:boolean;

 begin
  result:='';
  r.bottom:=r.bottom-subh;

  w:=0; sw:=0; h:=canvas.textheight('gh');

  linebreak:=false; imgbreak:=false;  linkbreak:=false;
  hotspot:=false;
  cr:=r;
  res:='';

  if (isPara) and not calc then
   begin
    pencolor:=canvas.pen.color;
    canvas.pen.color:=canvas.brush.color;
    canvas.rectangle(fr.left,r.top,fr.right,r.top+h);
   end;

  while (length(s)>0) and not linebreak and not imgbreak do
   begin
    tagpos:=pos('<',s);
    spacepos:=pos(' ',s);

    if (tagpos>0) and ((spacepos>tagpos) or (spacepos=0)) then
     begin
      su:=copy(s,1,tagpos-1);
     end
    else
     begin
       if (spacepos>0) then
        su:=copy(s,1,spacepos)
       else
        su:=s;
     end;

   {$IFDEF TMSDEBUG}
   outputdebugstring(pchar(su+ '.'));
   {$ENDIF}

   if (length(su)>0) then
    begin
     dsu:=su;
     srchpos:=0;

     while (iposfrom('&',dsu,srchpos)>0) do
      begin
       if not ireplacestring('&lt;','<',dsu,srchpos) then
        if not ireplacestring('&gt;','>',dsu,srchpos) then
         if not ireplacestring('&amp;','&',dsu,srchpos) then
          if not ireplacestring('&quot;','"',dsu,srchpos) then srchpos:=$FFFF;
      end;

     th:=canvas.textheight(dsu);

     if isSub and (subh<(th shr 2)) then subh:=(th shr 2);
     if isSup and (suph<(th shr 2)) then suph:=(th shr 2);

     if (th>h) then h:=th;

     stripval:=stripval+dsu;
     if not calc then
      begin
       {$IFDEF TMSDEBUG}
       outputdebugstring(pchar(dsu));
       {$ENDIF}

       if isSup then cr.bottom:=cr.bottom-suph;
       if isSub then cr.bottom:=cr.bottom+subh;

       cr.bottom:=cr.bottom-imgali;

       if isShad then
        begin
         offsetrect(cr,shadowoffset,shadowoffset);
         newcolor:=canvas.font.color;
         canvas.font.color:=ShadowColor;
         DrawText(canvas.handle,pchar(dsu),length(dsu),cr,DT_LEFT or DT_SINGLELINE or DT_EXTERNALLEADING or DT_BOTTOM  or DT_EXPANDTABS or DT_NOPREFIX or DT_NOCLIP);
         offsetrect(cr,-shadowoffset,-shadowoffset);
         canvas.font.color:=newcolor;
        end;

       DrawText(canvas.handle,pchar(dsu),length(dsu),cr,DT_LEFT or DT_SINGLELINE or DT_EXTERNALLEADING or DT_BOTTOM  or DT_EXPANDTABS or DT_NOPREFIX or DT_NOCLIP);
       DrawText(canvas.handle,pchar(dsu),length(dsu),cr,DT_LEFT or DT_CALCRECT or DT_EXPANDTABS or DT_NOPREFIX  or DT_EDITCONTROL or DT_EXTERNALLEADING);

       if anchor and (hyperlinks-1=focuslink) then focusanchor:=lastanchor;

       {$IFDEF TMSDEBUG}
       if anchor then
        outputdebugstring(pchar('drawrect for '+anchorval+'= ['+inttostr(cr.left)+':'+inttostr(cr.top)+'] ['+inttostr(cr.right)+':'+inttostr(cr.bottom)+'] @ ['+inttostr(xpos)+':'+inttostr(ypos)));
       {$ENDIF}

       cr.left:=cr.right;
       cr.right:=r.right;
       cr.bottom:=r.bottom;
       cr.top:=r.top;
      end
     else
      begin
       cr:=r; {reinitialized each time !}
       DrawText(canvas.handle,pchar(dsu),length(dsu),cr,DT_LEFT or DT_CALCRECT or DT_EXPANDTABS or DT_NOPREFIX or DT_EXTERNALLEADING);

       {modified for editing purposes}
       if (ypos>cr.top) and (ypos<cr.bottom) and (xpos>w) then {scan charpos here}
        begin
         er:=rect(w,cr.top,xpos,cr.bottom);
         fillchar(dtp,sizeof(dtp),0);
         dtp.cbSize:=sizeof(dtp);


         GetTextExtentExPoint(canvas.handle,pchar(dsu),length(dsu),xpos-w,@nnfit,nil,nnSize);

         {this will get the character pos of the insertion point}

         if nnfit=length(dsu) then inspoint:=inspoint+length(dsu) else
           begin
            inspoint:=inspoint+nnfit;
//            outputdebugstring(pchar('here ('+inttostr(xpos)+':'+inttostr(w)+') ['+dsu+'] - '+inttostr(inspoint)));
           end;
        end;
       {end of edit modify}

       w:=w+(cr.right-cr.left);

       if (xpos-cr.left>=w-cr.right+cr.left) and (xpos-cr.left<=w) and anchor then
         begin
          hotspot:=true;

//          if (ypos>r.bottom-imgali-canvas.textheight('gh')) and (ypos<r.bottom-imgali) then
          if (ypos>cr.top) and (ypos<cr.bottom) then
            begin
//              outputdebugstring(pchar('got anchor:'+inttostr(ypos)+':'+inttostr(r.bottom)));
              anchorval:=lastanchor;
              mouseinanchor:=true;
            end;
         end;
      end;

     if (w<r.right-r.left) or (r.right-r.left<=cr.right-cr.left) then
      begin
       res:=res+copy(s,1,length(su));
       delete(s,1,length(su));
       if su[length(su)]=' ' then sw:=canvas.textwidth(' ') else sw:=0;
      end
     else
      begin
       linebreak:=true;
       w:=w-(cr.right-cr.left);
      end;
    end;

  {v1.7 : added new functionality to display database field tags}
  s:=dbtagstrip(s);
  {v1.7}

  if not linebreak and (pos('<',s)=1) then
   begin
     if (pos('</',s)=1) then
       begin
        case upcase(s[3]) of
        'A':begin
             if not hoverstyle or (hoverlink=hyperlinks) then
               begin
                canvas.font.style:=canvas.font.style-[fsUnderline];
                if hovercolor<>clNone then
                  begin
                   canvas.brush.color:=hvrcolor;
                   if hvrcolor=clNone then canvas.brush.style:=bsClear;
                  end;
                if hoverfontcolor<>clNone then
                  begin
                   canvas.font.color:=hoverfontcolor;
                  end;
               end;
             if not selected then canvas.font.color:=oldfont.color;
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
                outputdebugstring(pchar('hotspot anchor '+lastanchor+' = ['+inttostr(hr.left)+':'+inttostr(hr.top)+'] ['+inttostr(hr.right)+':'+inttostr(hr.bottom)+']'));
                {$ENDIF}
                mouseinanchor:=false;
               end;

             if (focuslink=hyperlinks-1) then
              begin
               rr.right:=cr.left;
               rr.bottom:=cr.bottom-imgali;
               rr.top:=rr.bottom-canvas.textheight('gh');
               inflaterect(rr,1,0);
               if not calc then canvas.drawfocusrect(rr);
              end;

            end;
        'B':begin
             if ipos('</BL',s)=1 then
              canvas.font.color:=oldfont.color
             else
              canvas.font.style:=canvas.font.style-[fsBold];
            end;
        'S':begin
             if (upcase(s[4])='U') then
               begin
                isSup:=false;
                isSub:=false;
               end
             else
              if (upcase(s[4])='H') then
               isShad:=false
              else
               canvas.font.style:=canvas.font.style-[fsStrikeOut];
            end;
        'F':begin
             canvas.font.name:=oldfont.name;
             canvas.font.size:=oldfont.size;
             if not calc and not selected then
              begin
                canvas.font.color:=oldfont.color;
                canvas.brush.color:=bgcolor;
                if bgcolor=clNone then
                  begin
                   canvas.brush.style:=bsClear;
                  end;
              end;
            end;
        'I':begin
             canvas.font.style:=canvas.font.style-[fsItalic];
            end;
        'P':begin
             linebreak:=true;
             if not calc then
              begin
               canvas.brush.color:=paracolor;
               if paracolor=clNone then canvas.brush.style:=bsClear;
               isPara:=false;
              end;
            end;
        'U':begin
             if (ipos('</UL>',s)=1) and (listindex>0) then listindex:=listindex-1
             else
              canvas.font.style:=canvas.font.style-[fsUnderline];
            end;
        'R':begin
             EndRotated(canvas);
            end;

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
                 canvas.font.style:=canvas.font.style+[fsUnderline];
                 if hovercolor<>clNone then
                  begin
                   hvrcolor:=canvas.brush.color;
                   if canvas.brush.style=bsClear then hvrcolor:=clNone;
                   canvas.brush.color:=HoverColor;
                  end;
                 if hoverfontcolor<>clNone then
                  begin
                   hvrfntcolor:=canvas.font.color;
                   canvas.font.color:=hoverfontcolor;
                  end;
               end;


             if not selected and ((hoverfontcolor=clNone) or (hoverlink<>hyperlinks) or not hoverstyle) then canvas.font.color:=urlcolor;

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
             if ipos('<BR>',s)=1 then
               begin
                linebreak:=true;
                stripval:=stripval+#13;
               end;
             if ipos('<B>',s)=1 then
               begin
                canvas.font.style:=canvas.font.style+[fsBold];
               end;
             if ipos('<BLINK',s)=1 then
               begin
                //oldfont.color:=canvas.font.color;
                if not blink then canvas.font.color:=blnkcolor;
               end;
             if ipos('<BODY',s)=1 then
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
                      SelectClipRgn(canvas.handle, hrgn);

                      while (bmpy < fr.bottom-fr.top) do
                       begin
                        bmpx:=0;
                        while (bmpx < fr.right-fr.left) do
                         begin
                          canvas.draw(fr.left+bmpx,fr.top+bmpy,bmp);
                          bmpx:=bmpx+bmp.width;
                        end;
                        bmpy:=bmpy+bmp.height;
                       end;

                      finally
                      bmp.Free;
                      SelectClipRgn(canvas.handle, 0);
                      DeleteObject(hrgn);
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
                     canvas.brush.color:=text2color(ansilowercase(prop));
                   if (pos('#',prop)>0) then
                     canvas.brush.color:=hex2color(prop);

                  if not calc then
                   begin
                    bgcolor:=canvas.brush.color;
                    pencolor:=canvas.pen.color;
                    canvas.pen.color:=bgcolor;
                    canvas.rectangle(fr.left,fr.top,fr.right,fr.bottom);
                    canvas.pen.color:=pencolor;
                   end;
                  end;
                 end;
               end;

            end;
        'H':begin
             linebreak:=true;
             if not calc then
              begin
               pencolor:=canvas.pen.color;
               canvas.pen.color:=clblack;
               canvas.MoveTo(r.left,cr.bottom+1);
               canvas.Lineto(r.right,cr.bottom+1);
               canvas.pen.color:=pencolor;
              end;
            end;
        'I':begin
             if ipos('<I>',s)=1 then
               begin
                canvas.font.style:=canvas.font.style+[fsItalic];
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
                //oldfont.color:=canvas.font.color;
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

                tagtransp:=(pos('transp',tagprop)>0);

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
                        bmp.Transparent:=tagtransp;
                        fImages.GetBitmap(istrtoint(prop),bmp);
                        PrintBitmap(canvas,cr,bmp);
                        bmp.free;
                       end;
                    end;
                  end;

                if ipos('ssys:',prop)>0 then
                  begin
                   delete(prop,1,5);
                   bmpsize:=sysimage(canvas,cr.left,cr.top,prop,false,not calc,print,resfactor);
                  end;

                if ipos('lsys:',prop)>0 then
                  begin
                   delete(prop,1,5);
                   bmpsize:=sysimage(canvas,cr.left,cr.top,prop,true,not calc,print,resfactor);
                  end;

                if ipos('res://',prop)>0 then
                  begin
                    delete(prop,1,6);
                    if findresource(hinstance,pchar(prop),RT_BITMAP)<>0 then
                    begin
                    bmp:=tbitmap.Create;
                    bmp.Transparent:=tagtransp;
                    try
                     bmp.LoadFromResourceName(hinstance,prop);
                     if not calc and not print then canvas.Draw(cr.left,cr.top,bmp);
                     if not calc and print then
                       begin
                        cr.right:=cr.left+round(resfactor*bmpsize.x);
                        cr.bottom:=cr.top+round(resfactor*bmpsize.y);
                        PrintBitmap(canvas,cr,bmp);
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
                      bmp.Transparent:=tagtransp;
                      try
                      bmp.LoadFromFile(prop);
                      if not calc and not print then
                        begin
                         if (tagwidth>0) and (tagheight>0) then
                           begin
                            canvas.StretchDraw(rect(cr.left,cr.top,cr.left+tagwidth,cr.top+tagheight),bmp);
                           end
                         else
                           canvas.Draw(cr.left,cr.top,bmp);
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
                        PrintBitmap(canvas,cr,bmp);
                       end;

                      finally
                      bmp.Free;
                      end;
                     end;
                  end;

                 {$IFDEF DBAWARE}

                 if (ipos('fld:',prop)>0) and assigned(fFields) then
                  begin
                    delete(prop,1,4);

                    bmpsize.x:=tagwidth;
                    bmpsize.y:=tagheight;

                    bmp:=tbitmap.Create;
                    bmp.Transparent:=tagtransp;
                    bmp.assign(fFields.FieldByName(prop));

                    if not calc and not print then
                     begin
                      if (tagwidth>0) and (tagheight>0) then
                       begin
                        canvas.StretchDraw(rect(cr.left,cr.top,cr.left+tagwidth,cr.top+tagheight),bmp);
                       end
                      else
                        canvas.Draw(cr.left,cr.top,bmp);
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
                      PrintBitmap(canvas,cr,bmp);
                     end;
                    bmp.Free;

                  end;
                 {$ENDIF}

                 if (xpos>w) and (xpos<w+bmpsize.x) and
                  {added the vertical condition again}
                   (ypos>cr.top) and (ypos<cr.bottom) and anchor then
                   begin
                    hotspot:=true;
                    anchorval:=lastanchor;
                   end;

                 if print then
                  begin
                   bmpsize.x:=round(bmpsize.x*resfactor);
                   bmpsize.y:=round(bmpsize.y*resfactor);
                   {$IFDEF TMSDEBUG}
                   outputdebugstring(pchar('px:'+inttostr(bmpsize.x)));
                   outputdebugstring(pchar('py:'+inttostr(bmpsize.y)));
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
                      imgali:=h-canvas.textheight('gh');
                    end
                   else
                    begin
                     if (ipos('"middle',tagprop)>0) then
                      begin
                        imgali:=(h-canvas.textheight('gh')) shr 1;
                      end;
                    end;
                  end;
               end;
            end;
        'L':begin
             w:=w+12*listindex;
                //outputdebugstring(pchar(inttostr(listindex)));
             if linkbreak then imgbreak:=true else linkbreak:=true;
             cr.left:=cr.left+12*(listindex-1);
             if not calc then
              begin
               prop:=canvas.font.name;
               canvas.font.name:='Symbol';
               if odd(listindex) then
               drawtext(canvas.handle,'·',1,cr,0)
               else
               drawtext(canvas.handle,'o',1,cr,0);
               canvas.font.name:=prop;
              end;
             cr.left:=cr.left+12;
            end;
        'U':begin
             if ipos('<UL>',s)=1 then
               begin

                listindex:=listindex+1
               end
             else
              canvas.font.style:=canvas.font.style+[fsUnderline];
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
                   paracolor:=canvas.brush.color;
                   if canvas.brush.style=bsClear then paracolor:=clNone;
                   canvas.brush.color:=newcolor;
                   pencolor:=canvas.pen.color;
                   canvas.pen.color:=newcolor;
                   canvas.rectangle(fr.left,r.top,fr.right,r.bottom);
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
                 //oldfont.name:=canvas.font.name;
                 canvas.font.name:=prop;
                end;
              if (ipos(' color',tagprop)>0) and (selected=false) then
                begin
                 prop:=copy(tagprop,ipos('color',tagprop)+4,length(tagprop));
                 prop:=copy(prop,pos('"',prop)+1,length(prop));
                 prop:=copy(prop,1,pos('"',prop)-1);
                 //oldfont.color:=canvas.font.color;

                 if (pos('cl',prop)>0) then
                  begin
                   canvas.font.color:=text2color(ansilowercase(prop));
                  end;
                 if (pos('#',prop)>0) then
                  begin
                   canvas.font.color:=hex2color(prop);
                  end;
                end;
              if (ipos('bgcolor',tagprop)>0) and not calc and not selected then
                begin
                 prop:=copy(tagprop,ipos('bgcolor',tagprop)+4,length(tagprop));
                 prop:=copy(prop,pos('"',prop)+1,length(prop));
                 prop:=copy(prop,1,pos('"',prop)-1);

                 bgcolor:=canvas.brush.color;
                 if canvas.brush.style=bsClear then bgcolor:=clNone;

                 if (pos('cl',prop)>0) then
                  begin
                   canvas.brush.color:=text2color(ansilowercase(prop));
                  end;
                 if (pos('#',prop)>0) then
                  begin
                   canvas.brush.color:=hex2color(prop);
                  end;
                end;

              if (ipos('size',tagprop)>0) then
                begin
                 prop:=copy(tagprop,ipos('size',tagprop)+4,length(tagprop));
                 prop:=copy(prop,pos('"',prop)+1,length(prop));
                 prop:=copy(prop,1,pos('"',prop)-1);
                 //oldfont.size:=canvas.font.size;
                 canvas.font.size:=istrtoint(prop);
                end;
             end;

            end;
        'S':begin
              if (ipos('<S>',s)=1) then
               begin
                canvas.font.style:=canvas.font.style+[fsStrikeOut];
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
              StartRotated(canvas,indent);
            end;
         end;
       end;

     if (pos('>',s)>0) and not imgbreak then
       begin
        res:=res+copy(s,1,pos('>',s));
        delete(s,1,pos('>',s));
       end
      else if not imgbreak then delete(s,1,length(s));
    end;
  end;
  w:=w-sw;
  if (w>xsize) then xsize:=w;

  if (focuslink=hyperlinks-1) and (anchor) and not calc then
  begin
   rr.right:=cr.left;
   rr.bottom:=cr.bottom;
   inflaterect(rr,1,0);
   if not calc then canvas.drawfocusrect(rr);
   rr.left:=r.left+1;
   rr.top:=rr.bottom;
  end;

  result:=res;
 end;
 {$WARNINGS ON}

begin
 anchor:=false;
 oldfont:=tfont.Create;
 oldfont.Assign(canvas.font);
 drawfont:=tfont.Create;
 drawfont.Assign(canvas.font);
 calcfont:=tfont.Create;
 calcfont.Assign(canvas.font);
 olddrawfont:=tfont.Create;
 olddrawfont.Assign(canvas.font);
 oldcalcfont:=tfont.Create;
 oldcalcfont.Assign(canvas.font);
 blnkcolor:=canvas.brush.color;
 canvas.brush.color:=clNone;
 bgcolor:=clNone;
 paracolor:=clNone;
 isPara:=false; isShad:=false;

 result:=false;
 r:=fr;
 r.left:=r.left+1; {required to add offset for DrawText problem with first capital W letter}

 align:=taLeftJustify;

 xsize:=0; ysize:=0; hyperlinks:=0; hlcount:=0; listindex:=0; licount:=0;
 stripval:='';
 focusanchor:='';
 mouselink:=-1;
 mouseinanchor:=false;

 setbkmode(canvas.handle,TRANSPARENT);

 inspoint:=0;

 while length(s)>0 do
  begin
   oldfont.Assign(oldcalcfont);
   canvas.font.Assign(calcfont);
   oldanchor:=anchor;
   oldanchorval:=lastanchor;
   suph:=0; subh:=0; imgali:=0;
   isSup:=false; isSub:=false;

   su:=HTMLDrawLine(canvas,s,r,true,htmlwidth,htmlheight,subh,suph,imgali,align,xpos,ypos,hotspot);

   anchor:=oldanchor;
   lastanchor:=oldanchorval;

   calcfont.Assign(canvas.font);
   oldcalcfont.Assign(oldfont);

   dr:=r;
   case align of
   taCenter:dr.left:=r.left+((r.right-r.left-htmlwidth) shr 1);
   taRightJustify:dr.left:=r.right-htmlwidth;
   end;

   dr.bottom:= dr.top+htmlheight+subh+suph;

   if not checkheight then
    begin
    oldfont.Assign(olddrawfont);
    canvas.font.Assign(drawfont);

    hyperlinks:=hlcount;
    listindex:=licount;

    HTMLDrawLine(canvas,su,dr,checkhotspot,htmlwidth,htmlheight,subh,suph,imgali,align,xpos,ypos,hotspot);

    hlcount:=hyperlinks;
    licount:=listindex;

    if hotspot and (ypos>dr.bottom-imgali-canvas.textheight('gh')) and (ypos<dr.bottom-imgali) then
        result:=true;

    drawfont.Assign(canvas.font);
    olddrawfont.Assign(oldfont);
    end;

   r.top:=r.top+htmlheight+subh+suph;

   ysize:=ysize+htmlheight+subh+suph;

   if (r.top+htmlheight>r.bottom) and not checkheight then s:='';

  end;

 if (ysize=0) then ysize:=canvas.textheight('gh');
 inspoint:=inspoint shr 1;

 canvas.brush.color:=blnkcolor;
 canvas.font.assign(oldfont);
 oldfont.Free;
 drawfont.Free;
 calcfont.Free;
 olddrawfont.Free;
 oldcalcfont.Free;
end;

{$IFNDEF REMOVEDRAW}

function HTMLDraw(canvas:tcanvas;s:string;fr:trect;
                                 fImages:TImageList;
                                 xpos,ypos:integer;
                                 checkhotspot,checkheight,print,selected,blink:boolean;
                                 resfactor:double;
                                 urlcolor:tcolor;
                                 var anchorval,stripval:string;
                                 var xsize,ysize:integer):boolean;
var
 hyperlinks,mouselink:integer;
 focusanchor:string;
 r:trect;
begin
 result:=HTMLDrawEx(canvas,s,fr,fImages,xpos,ypos,-1,-1,1,checkhotspot,checkheight,print,selected,blink,false,
                    resfactor,urlcolor,clNone,clNone,clGray,anchorval,stripval,focusanchor,xsize,ysize,hyperlinks,mouselink,r);
end;

{$ENDIF}


{$IFDEF PARAMS}
function iposv(su,s:string;var vp:integer):integer;
begin
  result:=pos(uppercase(su),uppercase(s));
  vp:=result;
end;


function GetHREFValue(html,href:string;var value:string):boolean;
var
 lp:integer;
begin
 result:=false;
 while (iposv('href="',html,lp)>0) do
  begin
   delete(html,1,lp+5); {delete all before}
   if (iposv('"',html,lp)>0) then
    begin
     if CompareText(href,copy(html,1,lp-1))=0 then
      begin
       {href match - get the value now}
       delete(html,1,lp);
       if (iposv('>',html,lp)>0) then
        begin
         delete(html,1,lp);
         if (iposv('<',html,lp)>0) then
           begin
            value:=copy(html,1,lp-1);
            result:=true;
            break;
           end;
        end;
      end;
    end;
  end;
end;
{$ENDIF}


{$IFNDEF REMOVESTRIP}

function HTMLStrip(s:string):string;
var
 res:string;
 i:integer;
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
    begin
     delete(s,1,pos('>',s));
    end;
  end;
 result:=res+s;
end;

{$ENDIF}

{$IFDEF PARAMS}

function SetHREFValue(var html:string;href,value:string):boolean;
var
 h:string;
 p:string;
begin
 {get current value and do a stringreplace}

 result:=false;
 if GetHREFValue(html,href,h) then
  begin
   p:=copy(html,pos('href="'+href,html),length(html));
   p:=StringReplace(p,'>'+h+'</A','>'+value+'</A',[rfIgnoreCase]);


   html:=copy(html,1,pos('href="'+href,html)-1)+p;


   result:=true;
  end;
end;

{$ENDIF}



{************************************************************************}
{ TANIICON component                                                     }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ written by                                                             }
{ TMS Software                                                           }
{ copyright © 1998-2009                                                  }
{ Email : info@tmssoftware.com                                           }
{ Web : http://www.tmssoftware.com                                       }
{                                                                        }
{ The source code is given as is. The author is not responsible          }
{ for any possible damage done due to the use of this code.              }
{ The component can be freely used in any application. The complete      }
{ source code remains property of the author and may not be distributed, }
{ published, given or sold in any form as such. No parts of the source   }
{ code can be included in any other component or application without     }
{ written authorization of the author.                                   }
{************************************************************************}

{$DEFINE noDEBUGCOMP}                                         

{$I TMSDEFS.INC}

unit aniicon;

interface

uses
 Graphics, Windows, Classes, Controls, SysUtils,
 Messages, Dialogs, Forms, extctrls, Types;

const
 cklist:array[0..3] of ansichar = 'LIST';
 ckicon:array[0..3] of ansichar = 'ICON';
 ckrate:array[0..3] of ansichar = 'rate';
 ckseq:array[0..3] of ansichar = 'seq ';

 MAJ_VER = 1; // Major version nr.
 MIN_VER = 2; // Minor version nr.
 REL_VER = 0; // Release nr.
 BLD_VER = 3; // Build nr.

 // version history
 // 1.2.0.1 : fix for large icon sizes
 // 1.2.0.2 : Fixed : issue with Delphi 2009
 // 1.2.0.3 : Fixed : issue with unicode

const
 crMyAniCursor = 2000;

type
  ticonfile = array[1..766] of byte;

  rtag = record
          ckid:array[0..3] of ansichar;
          cksize:longint;
         end;

  tAniHeader = record
               cbSizeof:longint;
               cFrames:longint;
               cSteps:longint;
               cx,cy:longint;
               cBitCount,cPlanes:longint;
               jifRate:longint;
               fl:longint;
              end;

  ticondir = record
                reserved:word;
                cursortype:word;
                count:word;
               end;

  ticonentry = record
                  width:byte;
                  height:byte;
                  colors:byte;
                  rsrvd:byte;
                  hotspotx,hotspoty:word;
                  bytesinres:longint;
                  imgoffset:longint;
                 end;

  txormap = array[1..128] of longint;
  tandmap = array[1..32] of longint;

  tjifrates = array[0..127] of longint;
  tsequences = array[0..127] of longint;


  TAniIconFile = class(TGraphic)
    private
     iconhandles:array[0..63] of hicon;
     jifrates:tjifrates;
     sequences:tsequences;
     aniheader:taniheader;
     anistream:tmemorystream;
     FHasData:boolean;
     FCurFrame:integer;
     FJifCount:integer;
     FOnChange: TNotifyEvent;
     procedure AniDecode;
    protected
     procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
     function GetEmpty: Boolean; override;
     function GetHeight: integer; override;
     function GetWidth: integer; override;
     procedure SetHeight(Value: integer); override;
     procedure SetWidth(Value: integer); override;
     procedure ReadData(Stream: TStream); override;
     procedure WriteData(Stream: TStream); override;
     procedure Step;
     procedure SetFrame(i:integer);
     function GetStreamSize:integer;
     procedure StreamToTemp(filename:string);
    public      
     constructor Create; override;
     destructor Destroy; override;
     procedure Assign(Source: TPersistent); override;
     procedure LoadFromFile(const FileName: string); override;
     procedure LoadFromStream(Stream: TStream); override;
     procedure SaveToStream(Stream: TStream); override;
     procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle; var APalette: HPALETTE); override;
     procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE); override;
     property OnChange: TNotifyEvent read FOnChange write FOnChange;
     property HasData:boolean read fHasData write fHasData;
     function GetCursorHandle:thandle;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAniIcon = class(TCustomControl)
    private
     FAniFile:TAniIconFile;
     FAnimated:Boolean;
     FTimerRun:Boolean;
     FFrame:integer;
     FTransparent:boolean;
     FAnimateOnEnter: Boolean;
     FCenter:boolean;
     FButtonStyle:boolean;
     FHasMouse:boolean;
     FDragging:boolean;
     procedure SeTAniIconFile(newValue:TAniIconFile);
     procedure SetAnimated(SetOn:boolean);
     procedure SetFrame(f:integer);
     procedure SetCenter(const value:boolean);
     procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
     procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
     procedure WMEraseBkGnd(var Message:TMessage); message WM_EraseBkGnd;
     procedure WMTimer(var Msg: TWMTimer); message WM_Timer;
     procedure WMDestroy(var Message:TMessage); message WM_Destroy;
     function GetVersion: string;
     procedure SetVersion(const Value: string);
     function GetVersionNr: Integer;
    protected
     procedure Paint; override;
     procedure AniChanged(Sender: TObject);
     procedure CreateParams(var Params:TCreateParams); override;
     procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
                         X, Y: Integer); override;
     procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
     procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
                       X, Y: Integer); override;
    public
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
     property Canvas;
    published
     property AniFile:TAniIconFile read FAniFile write SeTAniIconFile;
     property Animated:Boolean read FAnimated write SetAnimated;
     property AnimateOnEnter:boolean read FAnimateOnEnter write FAnimateOnEnter;
     property Align;
     property Color;
     property DragCursor;
     property DragMode;
     property Enabled;
     property Frame:integer read FFrame write SetFrame;
     property ParentColor;
     property ParentShowHint;
     property PopupMenu;
     property ShowHint;
     property Visible;
     property OnClick;
     property OnDblClick;
     property OnDragDrop;
     property OnDragOver;
     property OnEndDrag;
     property OnMouseDown;
     property OnMouseMove;
     property OnMouseUp;
     property Transparent:boolean read FTransparent write FTransparent;
     property Center:boolean read fCenter write SetCenter;
     property ButtonStyle:boolean read fButtonStyle write fButtonStyle;
     property Version: string read GetVersion write SetVersion;
  end;

implementation

constructor TAniIconFile.Create;
begin
 inherited Create;
 fillchar(iconhandles,sizeof(iconhandles),0);
 FCurFrame:=0;
 FJifCount:=0;
 FHasData:=false;
 AniStream := TMemoryStream.Create;
end;

function TAniIconFile.GetStreamSize:integer;
begin
 result:=anistream.size;
end;

procedure TAniIconFile.StreamToTemp(filename:string);

begin
 if (anistream.size>0) then
  anistream.SaveToFile(filename);
end;

function TAniIconFile.GetCursorHandle:thandle;
var
  {$IFNDEF DELPHI_UNICODE}
  buf: array[0..MAX_PATH] of char;
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  buf: string;
  i: integer;
  {$ENDIF}
  s: string;

begin
  {$IFNDEF DELPHI_UNICODE}
  GetTempPath(sizeof(buf),buf);
  strcat(buf,'0.ANI');
  s := strpas(buf);
  {$ENDIF}

  {$IFDEF DELPHI_UNICODE}
  SetLength(buf, MAX_PATH);
  i := GetTempPath(Length(buf), PChar(buf));
  SetLength(buf, i);
  buf := buf + '0.ANI';
  s := string(buf);
  {$ENDIF}

  {$IFDEF TMSDEBUG}
  outputdebugstring(buf);
  {$ENDIF}

  Result := 0;
  if self.GetStreamSize<=0 then exit;

  self.StreamToTemp(s);

  result := LoadImage( 0,
  {$IFDEF DELPHI_UNICODE}
                 PChar(buf),
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
                 buf,
  {$ENDIF}
                 IMAGE_CURSOR,
                 0,
                 0,
                 LR_DEFAULTSIZE or LR_LOADFROMFILE);

 deletefile(s);
end;

procedure TAniIconFile.Assign(Source: TPersistent);
var
 i:integer;
begin
 {$IFDEF TMSDEBUG}
 messagedlg('in assign proc',mtInformation,[mbOK],0);
 {$ENDIF}
 if (source=nil) then
  begin
   fHasData:=false;
   Anistream.free;
   AniStream := TMemoryStream.Create;

   for i:=0 to 63 do
    begin
     if (iconhandles[i]>0) then
        begin
         DestroyIcon(iconhandles[i]);
         iconhandles[i]:=0;
       end;
    end;
  end
 else
  begin
   if Source is TAniIconFile then
     begin
      AniStream.LoadFromStream(TAniIconFile(Source).Anistream);
      fHasData:=true;
     end
   else
    inherited Assign(Source);
  end;
end;

procedure TAniIconFile.SetFrame(i:integer);
begin
 if (i<0) or (i>63) then i:=0;
 FCurFrame:=i;
 if (iconhandles[FCurFrame]=0) then FCurFrame:=0;
 FJifCount:=0;
end;

procedure TAniIconFile.Step;
begin
 inc(FJifCount);
 inc(FJifCount);
 if (FJifCount>jifrates[FCurFrame]) then
   begin
    FJifCount:=0;
    if (FCurFrame<63) then inc(FCurFrame) else FCurFrame:=0;
    if (iconhandles[FCurFrame]=0) then FCurFrame:=0;
    if Assigned(FOnChange) then FOnChange(Self);
   end;
end;

procedure TAniIconFile.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
 with ACanvas do
  begin
   if (iconhandles[FCurFrame]>0) then
     begin
      DrawIcon(handle,rect.left,rect.top,iconhandles[FCurFrame]);
     end;
  end;
end;

function TAniIconFile.GetEmpty: Boolean;
begin
  Result:=false;
end;

function TAniIconFile.GetHeight: integer;
begin
  Result:=32;
end;

function TAniIconFile.GetWidth: integer;
begin
  Result:=32;
end;

procedure TAniIconFile.SetHeight(Value: integer);
begin
end;

procedure TAniIconFile.SetWidth(Value: integer);
begin
end;

procedure TAniIconFile.SaveToClipboardFormat(var AFormat: Word; var AData: THandle; var APalette: HPALETTE);
begin

end;

procedure TAniIconFile.LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE);
begin

end;

procedure TAniIconFile.ReadData(Stream: TStream);
begin
 {$IFDEF TMSDEBUG}
 messagedlg('in readdata proc',mtInformation,[mbOK],0);
 {$ENDIF}
 if assigned(Stream) then
   begin
     {$IFDEF TMSDEBUG}
     messagedlg('in readdata proc',mtInformation,[mbOK],0);
     {$ENDIF}
     anistream.loadfromstream(stream);
     anidecode;
     fHasData:=true;
     if Assigned(FOnChange) then FOnChange(Self);
   end;
end;

procedure TAniIconFile.WriteData(Stream: TStream);
begin
 if assigned(Stream) then
   begin
     {$IFDEF TMSDEBUG}
     messagedlg('in writedata proc',mtInformation,[mbOK],0);
     {$ENDIF}
     anistream.savetostream(stream);
   end;
end;

destructor TAniIconFile.Destroy;
var
 i:integer;
begin
 i:=0;
 while (iconhandles[i]>0) and (i<=63) do
   begin
     DestroyIcon(iconhandles[i]);
     inc(i);
   end;
 AniStream.Free;                                
 inherited Destroy;
end;

procedure TAniIconFile.LoadFromFile(const FileName: string);
begin
 if (UpperCase(ExtractFileExt(Filename))='.ANI') and FileExists(Filename) then
  begin
   {$IFDEF TMSDEBUG}
   messagedlg('stream.loadfromfile - start',mtInformation,[mbOK],0);
   {$ENDIF}
   AniStream.LoadFromFile(Filename);
   {$IFDEF TMSDEBUG}
   messagedlg('stream.loadfromfile - done',mtInformation,[mbOK],0);
   {$ENDIF}

   AniDecode;
   if Assigned(FOnChange) then
     FOnChange(Self);
     
   FHasData := true;
   {
   LoadFromStream(AniStream);
   }
  end;
  {
  TriggerChangeEvent;
  }
end;

procedure TAniIconFile.SaveToStream(Stream:TStream);
begin
  if Assigned(Stream) then
  begin
    AniStream.SaveToStream(Stream);
    {$IFDEF TMSDEBUG}
    messagedlg('save to stream done',mtInformation,[mbOK],0);
    {$ENDIF}
  end;
end;

procedure TAniIconFile.LoadFromStream(Stream:TStream);
begin
  if Assigned(Stream) then
  begin
    AniStream.LoadFromStream(Stream);
    {$IFDEF TMSDEBUG}
    messagedlg('load from stream done',mtInformation,[mbOK],0);
    {$ENDIF}
    AniDecode;
    fHasData:=true;
  end;
  {
  TriggerChangeEvent;
  }
end;


procedure TAniIconFile.AniDecode;
var
 tag:rtag;
 name:array[0..255] of char;
 i:integer;
 icondir:ticondir;
 iconentry:ticonentry;
 icondata:array[0..20000] of byte;
 s:string;
 {$IFDEF TMSDEBUG}
 numread:integer;
 {$ENDIF}
 ckids:string;

label
 readhdr;

 function mstrpas(l:pchar):string;
 var
  i:integer;
  s:string;
 begin
  s:='';
  for i:=0 to 3 do
     begin
       if (l^>#32) then s:=s+l^;
       inc(l);
     end;
  mstrpas:=s;
 end;

begin
  {$IFDEF TMSDEBUG}
  messagedlg('stream size is = '+inttostr(anistream.size),mtInformation,[mbOK],0);
  {$ENDIF}

  anistream.position:=0;
  if anistream.size=0 then exit;

  anistream.readbuffer(tag,8);
  anistream.readbuffer(tag,4);
  anistream.readbuffer(tag,8);

  ckids := string(tag.ckid);
  if (pos('anih',mstrpas(pchar(ckids)))>0) then goto readhdr;
  anistream.readbuffer(tag,4);
  anistream.readbuffer(tag,8);
  ckids := string(tag.ckid);
  if (pos('anih',mstrpas(pchar(ckids)))>0) then goto readhdr;
  fillchar(name,sizeof(name),0);
  if odd(tag.cksize) then inc(tag.cksize);
  anistream.readbuffer(name,tag.cksize);

  s:=strpas(name);

  {$IFDEF TMSDEBUG}
  messagedlg('anifile name = '+s,mtInformation,[mbOK],0);
  {$ENDIF}
  anistream.readbuffer(tag,8);
  fillchar(name,sizeof(name),0);

  if odd(tag.cksize) then inc(tag.cksize);
  anistream.readbuffer(name,tag.cksize);
  anistream.readbuffer(tag,8);
 readhdr:
  {$IFDEF TMSDEBUG}
  messagedlg('start reading aniheader',mtInformation,[mbOK],0);
  {$ENDIF}
  anistream.readbuffer(aniheader,sizeof(taniheader));
  for i:=0 to 80 do jifrates[i]:=aniheader.jifrate;
  anistream.readbuffer(tag,8);
  ckids := string(tag.ckid);
  if (pos('rate',mstrpas(pchar(ckids)))>0) then
    begin
      anistream.readbuffer(jifrates,tag.cksize);
      anistream.readbuffer(tag,8);
    end;
  ckids := string(tag.ckid);
  if (pos('seq',mstrpas(pchar(ckids)))>0) then
    begin
      anistream.readbuffer(sequences,tag.cksize);
      anistream.readbuffer(tag,8);
    end;
  ckids := string(tag.ckid);
  if (pos('LIST',mstrpas(pchar(ckids)))>0) then
    begin
      anistream.readbuffer(tag,4);

      for i:=0 to 63 do
       begin
        if (iconhandles[i]>0) then
           begin
            DestroyIcon(iconhandles[i]);
            iconhandles[i]:=0;
           end;
       end;
      {$IFDEF TMSDEBUG}
      numread:=0;
      {$ENDIF}

      {limit max. nr. of pictures}
      if (aniheader.cFrames>64) then aniheader.cFrames:=64;

      for i:=1 to aniheader.cframes do
       begin
         anistream.readbuffer(tag,8);
         {$IFDEF TMSDEBUG}
         messagedlg('tag = '+mstrpas(tag.ckid)+' size = '+inttostr(tag.cksize)+
                    ' stream position = '+inttostr(anistream.position),mtInformation,[mbOK],0);
         {$ENDIF}
         if (tag.cksize<766) then break;
         anistream.readbuffer(icondir,sizeof(icondir));
         anistream.readbuffer(iconentry,sizeof(iconentry));
         if (iconentry.imgoffset>22) then
           begin
            anistream.readbuffer(icondata,iconentry.imgoffset-22);
            tag.ckSize:=tag.CkSize-(iconentry.imgoffset-22);
           end;
  
         anistream.readbuffer(icondata,tag.ckSize-22);
         IconHandles[i-1]:=CreateIconFromResource(@icondata,tag.ckSize-22,true,$00030000);

         {$IFDEF TMSDEBUG}
         inc(numread);
         {$ENDIF}
         
         {
         anistream.readbuffer(icondata,744);
         IconHandles[i-1]:=CreateIconFromResource(@icondata,744,true,$00030000);
         inc(numread);
         tag.Cksize:=tag.Cksize-766;
         if (tag.Cksize>0) then anistream.readbuffer(icondata,tag.Cksize);
         }

       end;
      {$IFDEF TMSDEBUG}
      messagedlg('icons read = '+inttostr(numread),mtInformation,[mbOK],0);
      {$ENDIF}

      FCurFrame:=0;
      FJifCount:=0;
    end;
end;

constructor TAniIcon.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FAniFile:=TAniIconFile.Create;
  FAniFile.OnChange:=AniChanged;
  FAnimated:=false;
  FTimerRun:=false;
  width:=32;
  height:=32;
  ControlStyle:= ControlStyle - [csOpaque];
end;

procedure TAniIcon.CreateParams(var Params:TCreateParams); 
begin
 Inherited CreateParams(Params);
 Params.ExStyle:= Params.ExStyle or WS_EX_TRANSPARENT;
end;

destructor TAniIcon.Destroy;
begin
  FAniFile.Free;
  inherited Destroy;
end;

procedure TAniIcon.WMDestroy(var Message:TMessage);
begin
 if FTimerRun then KillTimer(handle,1);
 inherited;
end;

procedure TAniIcon.WMEraseBkGnd(var Message:TMessage);
begin
 if FTransparent then  message.result:=1 else inherited;
end;

procedure TAniIcon.WMTimer(var Msg: TWMTimer);
begin
 FAniFile.Step;
end;


procedure TAniIcon.AniChanged(Sender: TObject);
begin
  {$IFDEF TMSDEBUG}
  {
  messagedlg('AniChanged proc.',mtInformation,[mbok],0);
  }
  {$ENDIF}
  Repaint;
end;

procedure TAniIcon.Paint;
var
 r:trect;

begin
with Canvas do
 begin
  r.left:=self.left;
  r.top:=self.top;
  r.right:=self.left+width;
  r.bottom:=self.top+height;

  if (self.fTransparent) then
   begin
    invalidaterect((parent as TWinControl).handle,@r,true);
    updatewindow((parent as TWinControl).handle);
   end;

  if fCenter then
   Draw((self.width-32) shr 1,(self.height-32) shr 1,FAniFile)
  else
   Draw(0,0,FAniFile);

  if fButtonStyle then
   begin
    r.left:=0;
    r.right:=self.width;
    r.top:=0;
    r.bottom:=self.height;

    if fDragging then
        Frame3D(canvas,r,clGray,clWhite,1)
    else
    if fHasMouse then
      Frame3D(canvas,r,clWhite,clGray,1);
   end;

  if (csDesigning in ComponentState) then
   begin
    r.left:=0;
    r.right:=self.width;
    r.top:=0;
    r.bottom:=self.height;
    pen.Style:=psDot;
    pen.color:=clBlack;
    brush.style:=bsClear;
    Polyline([Point(0, 0), Point(self.width-1, 0),
    Point(self.width-1,self.height-1), Point(0,self.height-1), Point(0,0)]);
   end;
 end;
end;

procedure TAniIcon.SetCenter(const value:boolean);
begin
 fCenter:=value;
 self.repaint;
end;


procedure TAniIcon.SetFrame(f:integer);
begin
with Canvas do
 begin
  FAniFile.SetFrame(f);
  FFrame:=FAniFile.FCurFrame;
  repaint;
 end;
end;

procedure TAniIcon.SetAnimated(seton:boolean);
begin
 FAnimated:=seton;
 if (FAnimated<>FTimerRun) then
    begin
     if FTimerRun then
        begin
          KillTimer(handle,1);
          FTimerRun:=false;
        end
     else
        begin
          SetTimer(handle,1,20,nil);
          FTimerRun:=true;
        end;
    end;
end;

procedure TAniIcon.SeTAniIconFile(newValue:TAniIconFile);
begin
  {$IFDEF TMSDEBUG}
  messagedlg('in assign',mtInformation,[mbOK],0);
  {$ENDIF}
  FAniFile.Assign(newvalue);
  {$IFDEF TMSDEBUG}
  messagedlg('after assign',mtInformation,[mbOK],0);
  {$ENDIF}
  Invalidate;
end;



procedure TAniIcon.CMMouseEnter(var Msg: TMessage);
begin
 inherited;
 if Assigned(AniFile) and AnimateOnEnter and Enabled then Animated := True;
 fHasMouse:=true;
 self.repaint;
end;

procedure TAniIcon.CMMouseLeave(var Msg: TMessage);
begin
 inherited;
 if Assigned(AniFile) and AnimateOnEnter and Enabled then Animated := false;
 fHasMouse:=false;
 self.repaint;
end;

procedure TAniIcon.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
 inherited MouseDown(Button,shift,x,y);
 if (Button=mbLeft) then
  begin
   fDragging:=true;
    if fButtonStyle then self.repaint;
  end;
end;

procedure TAniIcon.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
 inherited MouseMove(Shift, X, Y);
end;

procedure TAniIcon.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
 inherited MouseUp(button,shift,x,y);
 fDragging:=false;
 if fButtonStyle then self.repaint;
end;

function TAniIcon.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAniIcon.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAniIcon.SetVersion(const Value: string);
begin

end;

end.


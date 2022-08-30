{********************************************************************}
{ TPARAMETERLISTBOX component                                        }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ TMS Software                                                       }
{ copyright © 1998-2013                                              }
{ Email : info@tmssoftware.com                                       }
{ Web : http://www.tmssoftware.com                                   }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the author and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit paramlst;

interface

{$I TMSDEFS.INC}

uses
  Stdctrls, Graphics, Windows, Messages, Classes, Forms, Controls,
  Sysutils, Dialogs, Inifiles, Registry, Types;


const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 9; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release


type
 TControlClickParameter = record
                            paramrect:trect;
                            mousex,mousey:integer;
                            index:integer;
                          end;

 TParameterClickEvent = procedure(Sender:TObject;AIndex:longint;Name:string;var Value:string) of object;

 TComboSelectEvent = procedure(Sender:TObject;AIndex:longint;OldValue,NewValue:string) of object;

 TShowListEvent = procedure(Sender:TObject;AIndex:longint;Name:string;var Value:string;
                            var showlist:boolean) of object;
 TControlClickEvent = procedure(Sender:TObject;param:tcontrolclickparameter;Name:string;var Value:string;
                                var showcontrol:boolean) of object;

 TParameterListBox = class;

{********************************
 Helper listbox object
 Handles escape, return, click
*********************************}

 TPopupListBox = class(TListBox)
 private
   fparamidx:integer;
   fOwnerList:TParameterListBox;
   procedure WMKeyDown(var Msg:TWMKeydown); message wm_keydown;
   procedure WMLButtonUp(var Message: TWMLButtonDown); message WM_LBUTTONUP;
   procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
 protected
   procedure CreateParams(var Params:TCreateParams); override;
   procedure SizeDropDownWidth;
 public
   property OwnerList:TParameterListBox read fOwnerList write fOwnerList;
 published
   property ParamIdx:integer read fParamIdx write fParamIdx;
 end;

  TParamLocation = (lcRegistry,lcInifile);

  TSaveParams = class(TPersistent)
  private
    FAutoSave: boolean;
    FAutoLoad: boolean;
    FKey : string;
    FSection : string;
    FApp : string;
    {$IFDEF WIN32}
    FLocation : TParamLocation;
    {$ENDIF}
    FOwner: TParameterListBox;
  public
    constructor Create(aOwner:TParameterListBox);
    destructor Destroy; override;
  published
    property App:string read fApp write fApp;
    property AutoSave:boolean read FAutoSave write FAutoSave;
    property AutoLoad:boolean read FAutoLoad write FAutoLoad;
    property Key:string read FKey write FKey;
    property Section:string read FSection write FSection;
    {$IFDEF WIN32}
    property Location:TParamLocation read fLocation write fLocation;
    {$ENDIF}
  end;

 {$IFDEF DELPHIXE2_LVL}
 [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
 {$ENDIF}
 TParameterListBox = class(TListBox)
  private
    cursorhand:boolean;
    origcursor:tcursor;
    highlight:boolean;
    highidx:integer;
    highoffs:integer;
    FOnParameterClick:TParameterClickEvent;
    FOnParameterActive:TParameterClickEvent;
    FOnControlClick:TControlClickEvent;
    FOnComboSelect:TComboSelectEvent;
    FOnShowList:TShowListEvent;
    FDropWidth:integer;
    FDropHeight:integer;
    FAutoDropWidth:boolean;
    FNameColor:tcolor;
    FValueColor:tcolor;
    FListBox:tPopupListbox;
    FStartDelimiter:string;
    FEndDelimiter:string;
    FDelimiter:string;
    FSaveParams:TSaveParams;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure WMMouseMove(var Msg:TWMMouseMove); message wm_mousemove;
    procedure WMLButtonUp(var Msg:TWMLButtonDown); message wm_lbuttonup;
    procedure WMKeyDown(var Msg:TWMKeydown); message wm_keydown;
    procedure WMDestroy(var Message:TMessage); message wm_DESTROY;
    function HasParam(s:string):boolean;
    function GetParamMax:integer;
    function GetParamMin:integer;
    function GetParamPos(Idx:integer):integer;
    function GetParamIdx(s:string):integer;
    function GetParamName(s:string):string;
    function GetParamValue(s:string):string;
    function GetFirstText(s:string):string;
    function GetLastText(s:string):string;
    function SetParamValue(i:integer;s,p:string):string;
    function GetParamValueName(i:integer;var name,value:string):integer;
    procedure SetParam(i:Integer;const Value:string);
    function GetParam(i:integer):string;
    procedure SetNameColor(acolor:tcolor);
    procedure SetValueColor(acolor:tcolor);
    procedure SetDelimiter(const Value: string);
    procedure SetActive(value:integer);
    function GetActive:integer;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    function GetVersionNr: Integer; virtual;
    procedure DrawItem(Index: Integer; Rect: TRect;State: TOwnerDrawState); override;
    procedure Loaded; override;
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(aOwner:tComponent); override;
    destructor Destroy; override;
    property Parameter[i:integer]:string read getParam write setParam;
    property Listbox:TPopupListbox read FListbox;
    procedure SaveParams;
    procedure LoadParams;
    procedure InitParams;
  published
    property OnParameterClick:TParameterClickEvent read FOnParameterClick write FOnParameterClick;
    property OnParameterActive:TParameterClickEvent read FOnParameterActive write FOnParameterActive;
    property OnControlClick:TControlClickEvent read FOnControlClick write FOnControlClick;
    property OnShowList:TShowListEvent read FOnShowList write FOnShowList;
    property OnComboSelect:TComboSelectEvent read fOnComboSelect write fOnComboSelect;
    property NameColor:tcolor read FNameColor write SetNameColor;
    property ValueColor:tcolor read FValueColor write SetValueColor;
    property Delimiter:string read fDelimiter write SetDelimiter;
    property ActiveParameter:integer read GetActive write SetActive;
    property SaveParam:TSaveParams read fSaveParams write fSaveParams;
    property DropWidth:integer read fDropWidth write fDropWidth;
    property DropHeight:integer read fDropHeight write fDropHeight;
    property AutoDropWidth:boolean read fAutoDropWidth write fAutoDropWidth;
    property Version: string read GetVersion write SetVersion;
 end;

implementation

{$IFDEF TMSDEBUG}
procedure OutputDebugStr(s:string);
var
 buf:array[0..255] of char;
begin
 strpcopy(buf,s);
 OutputDebugString(buf);
end;
{$ENDIF}


procedure TParameterListBox.SetNameColor(acolor:tcolor);
begin
 FNameColor:=acolor;
 self.repaint;
end;

procedure TParameterListBox.SetValueColor(acolor:tcolor);
begin
 FValueColor:=acolor;
 self.repaint;
end;

function TParameterListBox.GetFirstText(s:string):string;
begin
 GetFirstText:=copy(s,1,pos(FStartDelimiter,s)-1);
end;

function TParameterListBox.GetLastText(s:string):string;
begin
 GetLastText:=copy(s,pos(FEndDelimiter,s)+1,255);
end;

function TParameterListBox.HasParam(s:string):boolean;
begin
 HasParam:=(pos(FStartDelimiter,s)>0) and
           (pos(FEndDelimiter,s)>0) and
           (pos(FEndDelimiter,s)>pos(FStartDelimiter,s));
end;

function TParameterListBox.GetParamPos(Idx:integer):integer;
var
 j,k:integer;
 s:string;
begin
 result:=-1;
 if self.items.count<=0 then exit;
 for j:=0 to self.items.count-1 do
  begin
   s:=self.items[j];
   while self.HasParam(s) do
    begin
     k:=self.getparamidx(s);
     if (Idx=k) then
      begin
       result:=j;
       break;
      end;
     s:=self.GetLastText(s);
    end;
  end;
end;

function TParameterListBox.GetParamMax:integer;
var
 j,k:integer;
 s:string;
begin
 result:=-1;
 if self.items.count<=0 then exit;
 for j:=0 to self.items.count-1 do
  begin
   s:=self.items[j];
   while self.HasParam(s) do
    begin
     k:=self.getparamidx(s);
     if (k>result) then
      begin
       result:=k;
//       break;
      end;
     s:=self.GetLastText(s);
    end;
  end;
end;

function TParameterListBox.GetParamMin:integer;
var
 j,k:integer;
 s:string;
begin
 result:=32767;

 if self.items.count<=0 then exit;
 for j:=0 to self.items.count-1 do
  begin
   s:=self.items[j];
   while self.HasParam(s) do
    begin
     k:=self.getparamidx(s);
     if (k>0) and (k<result) then
      begin
       result:=k;
       break;
      end;
     s:=self.GetLastText(s);
    end;
  end;
end;

function TParameterListBox.GetParamIdx(s:string):integer;
var
 value,code:integer;
begin
 GetParamIdx:=-1;
 delete(s,1,pos(FStartDelimiter,s));
 if pos(',',s)>0 then
   begin
     s:=copy(s,1,pos(',',s)-1);
     val(s,value,code);
     if code<>0 then GetParamIdx:=-1 else GetParamIdx:=value;
   end;
end;

function TParameterListBox.GetParamName(s:string):string;
begin
 GetParamName:='';
 delete(s,1,pos(FStartDelimiter,s));
 if pos(',',s)>0 then
  begin
   delete(s,1,pos(',',s));
   if pos(',',s)>0 then
     begin
      GetParamName:=copy(s,1,pos(',',s)-1);
     end;
  end;
end;

function TParameterListBox.GetParamValue(s:string):string;
begin
 GetParamValue:='';
 delete(s,1,pos(FStartDelimiter,s));
 if pos(',',s)>0 then    { (idx,name,value)  }
  begin
   delete(s,1,pos(',',s));
   if pos(',',s)>0 then
     begin
      delete(s,1,pos(',',s));
      GetParamValue:=copy(s,1,pos(FEndDelimiter,s)-1);
     end;
  end;
end;

function TParameterListBox.SetParamValue(i:integer;s,p:string):string;
var
 s1:string;
begin
 SetParamValue:=s;

 if (pos(',',s)>0) then
  begin
   s1:=copy(s,1,pos(',',s));
   delete(s,1,pos(',',s));
   if (pos(',',s)>0) then
    begin
     s1:=s1+copy(s,1,pos(',',s));
     delete(s,1,pos(FEndDelimiter,s));
     s1:=s1+p+FEndDelimiter;
     s1:=s1+s;
     setparamvalue:=s1;
    end;
  end;
end;


function TParameterListBox.GetParamValueName(i:integer;var name,value:string):integer;
var
 j,k:integer;
 s,s1,s2:string;
 w:integer;

begin
 result:=0;
 if self.items.count<=0 then exit;

 for j:=0 to self.items.count-1 do
  begin
   s:=self.items[j];
   w:=0;
   while self.HasParam(s) do
    begin
     k:=self.getparamidx(s);

     s1:=self.GetFirstText(s);
     if self.GetParamValue(s)='' then
       s2:=s1+self.getParamName(s)
     else
       s2:=s1+self.getParamValue(s);


     if (i=k) then
      begin
       name:=self.GetParamName(s);
       value:=self.GetParamValue(s);
       result:=w+canvas.textwidth(s1);
       break;
      end;
     s:=self.GetLastText(s);
     w:=w+canvas.textwidth(s2);
    end;
  end;
end;

procedure TParameterListBox.SetParam(i:Integer;const Value:string);
var
 j,k:integer;
 s,su,prefix:string;

begin
 if self.items.count<=0 then exit;

 for j:=0 to self.items.count-1 do
  begin
   prefix:='';
   s:=self.items[j];
   while self.HasParam(s) do
    begin
     k:=self.getparamidx(s);
     if (i=k) then
      begin
       items[j]:=prefix+self.setparamvalue(i,s,value);
       break;
      end;

     su:=self.GetLastText(s);
     prefix:=prefix+system.copy(s,1,length(s)-length(su));
     s:=su;
    end;
  end;
end;

function TParameterListBox.GetParam(i:integer):string;
var
 j:integer;
 s:string;
begin
 GetParam:='';
 if self.items.count<=0 then exit;
 for j:=0 to self.items.count-1 do
  begin
   s:=self.items[j];
   while self.HasParam(s) do
    begin
     if (i=self.getparamidx(s)) then
      begin
       GetParam:=self.getparamvalue(s);
      end;
     s:=self.GetLastText(s);
    end;
  end;
end;


constructor TParameterListBox.Create(aOwner:tComponent);
begin
 inherited Create(aOwner);
 Style:=lbOwnerDrawFixed;
 highlight:=false;
 highidx:=-1;
 highoffs:=-1;
 cursorhand:=false;
 FNameColor:=clTeal;
 FValueColor:=clBlack;
 FSaveParams:=TSaveParams.Create(self);
 FStartDelimiter:='(';
 FEndDelimiter:=')';
 FDelimiter:='()';
 FAutoDropWidth:=true;
 FDropWidth:=100;
 FDropHeight:=100;

 if not (csDesigning in ComponentState) then
  begin
   Flistbox:=tPopuplistbox.create(self);
   Flistbox.parent:=self;
   Flistbox.visible:=false;
   FListBox.Cursor:=crDefault;
   FListBox.Width:=fDropWidth;
   FListBox.Height:=fDropHeight;
  end;
end;

destructor TParameterListBox.Destroy;
begin
 fSaveParams.Free;
 if not (csDesigning in ComponentState) then Flistbox.free;
 inherited Destroy;
end;

procedure TParameterListBox.DoEnter;
begin
 inherited DoEnter;
 if (ActiveParameter=-1) then ActiveParameter:=GetParamMin
 else ActiveParameter:=ActiveParameter;
end;

procedure TParameterListBox.DoExit;
begin
 inherited DoExit;
 {force unfocused repaint}
 ActiveParameter:=ActiveParameter;
end;

procedure TParameterListBox.WMDestroy(var Message:tMessage);
begin
 if fSaveParams.fAutoSave then SaveParams;
 inherited;
end;


procedure TParameterListBox.Loaded;
begin
 inherited Loaded;
 origcursor:=self.Cursor;
 if not (csDesigning in ComponentState) then flistbox.visible:=false;

 if fSaveParams.fAutoLoad then
  LoadParams;
end;

procedure TParameterListBox.DrawItem(Index: Integer; Rect: TRect;State: TOwnerDrawState);
var
 hght:integer;

  procedure DrawClickableTextNew(canvas:tcanvas;x,y:integer;s:string;highlight:boolean);
  var
   s1,s2,sp:string;
   r:trect;
   brcolor,fntcolor:tcolor;

  begin
   {'<> separators'}
   r.top:=y;
   r.bottom:=r.top+hght;
   r.left:=x;

   while (self.HasParam(s)) do
    begin
     if (self.GetParamValue(s)='') then
        sp:=GetParamName(s)
     else
        sp:=GetParamValue(s);

     s1:=GetFirstText(s); {text before param}
     s2:=GetLastText(s);  {text after param}

     r.right:=r.left+canvas.textwidth(s1);
     x:=r.left;
     canvas.textrect(r,x,y,s1); {draw first text}

     fntcolor:=canvas.font.color;
     brcolor:=canvas.brush.color;

     r.left:=r.left+canvas.textwidth(s1);
     r.right:=r.left+canvas.textwidth(sp);
     x:=r.left;


     if highlight and (highoffs=GetParamIdx(s)) then
       begin
        if self.getparamvalue(s)='' then
          canvas.brush.color:=FNameColor
        else
          canvas.brush.color:=FValueColor;
        canvas.font.color:=brcolor;
        end
     else
       begin
        if self.getparamvalue(s)='' then
          canvas.font.color:=FNameColor
        else
          canvas.font.color:=FValueColor;
       end;

     canvas.font.style:=canvas.font.style + [fsUnderline];

     r.right:=r.right+1;
     canvas.textrect(r,x,y,sp);

     if highlight and (highoffs=GetParamIdx(s)) then
       begin
        if GetFocus=self.handle then DrawFocusRect(canvas.handle,r);
       end;

     r.left:=r.left+canvas.textwidth(sp)+1;

     canvas.brush.color:=brcolor;
     canvas.font.color:=fntcolor;
     canvas.font.style:=canvas.font.style - [fsUnderline];;

     s:=s2;
   end;

  if (s<>'') then
   begin
     canvas.font.color:=clBlack;
     r.right:=r.left+canvas.textwidth(s);
     x:=r.left;
     canvas.textrect(r,x,y,s);
   end;
 end;

begin
  hght:=sendmessage(handle,lb_getitemheight,0,0);
  DrawClickableTextNew(canvas,rect.left,rect.top,items[index],highlight and (index=highidx));
end;

procedure TParameterListBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
    {$IFDEF WIN32}

      State := TOwnerDrawState(LongRec(itemState).Lo);

    {$ENDIF}
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    if Integer(itemID) >= 0 then DrawItem(itemID, rcItem, State);
    Canvas.Handle := 0;
  end;
end;

procedure TParameterListBox.WMMouseMove(var Msg:TWMMouseMove);
var
 idx,hght,clk,wdth:longint;
 s,s1,s2,ps1,ps2:string;
 newhighlight:boolean;
 newhighidx:integer;
 newhighoffs:integer;
 r:trect;

begin
  inherited;
  if flistbox.visible then
    Exit;

  idx := SendMessage(handle,lb_gettopindex,0,0);
  hght := SendMessage(handle,lb_getitemheight,0,0);

  wdth := 0;
  clk := (msg.ypos div hght) + idx;
  newhighlight:=false;
  newhighidx:=highidx;
  newhighoffs:=-1;

  if (clk<items.count) and (clk>=0) then
  begin
    s := Items[clk];
    if HasParam(s) then
    while HasParam(s) do
    begin
      s1:=self.GetFirstText(s);
      ps1:=self.GetParamName(s);
      ps2:=self.GetParamValue(s);

      if self.GetParamValue(s)='' then
        s2:=s1+self.getParamName(s)
      else
        s2:=s1+self.getParamValue(s);

      if (msg.xpos>canvas.TextWidth(s1)+wdth) and
         (msg.xpos<canvas.TextWidth(s2)+wdth) then
        begin {lower Delphi version!!}
         {$IFDEF CURSORRESOURCE}
         if not cursorhand then Cursor := crHandCur;
         {$ELSE}
         if not cursorhand then Cursor := crHandPoint;
         {$ENDIF}
         newhighlight:=true;
         newhighoffs:=self.GetParamIdx(s);
         cursorhand:=true;
         newhighidx:=clk;
         break;
        end;
      s:=self.GetLastText(s);
      wdth:=wdth+canvas.TextWidth(s2);
     end;
   end;

  if not newhighlight and cursorhand then
   begin
     Cursor := OrigCursor;
     cursorhand := false;
   end;

  if (newhighlight<>highlight) or (newhighidx<>highidx) then
    begin
     if (highidx>=0) and (highidx<self.items.Count) then
      begin
       SendMessage(Handle,lb_getitemrect,highidx,LParam(@r));
       InvalidateRect(Handle,@r,false);
      end;

     highlight:=newhighlight;
     highidx:=newhighidx;
     highoffs:=newhighoffs;

     if (highidx>=0) and (highidx<self.items.Count) then
      begin
       SendMessage(Handle,lb_getitemrect,highidx,LParam(@r));
       InvalidateRect(Handle,@r,false);
      end;

     if not highlight then
        begin
         highoffs:=-1;
         ps1:=''; ps2:='';
        end;
     if Assigned(FOnParameterActive) then
       FOnParameterActive(self,highoffs,ps1,ps2);
    end;
end;

procedure TParameterListBox.WMKeyDown(var Msg:TWMKeydown);
var
 s1,s2:string;
 showlist:boolean;
 hght,idx,w:integer;
begin
 inherited;

 if msg.CharCode in [vk_right,vk_down] then
  begin
   if ActiveParameter<GetParamMax then
      ActiveParameter:=ActiveParameter+1;
  end;

 if msg.CharCode in [vk_left,vk_up] then
  begin
   if ActiveParameter>GetParamMin then
      ActiveParameter:=ActiveParameter-1;
  end;

 if (msg.CharCode in [vk_space,vk_return]) and (highoffs>0) then
  begin
   w:=GetParamValueName(highoffs,s1,s2);
   showlist:=false;
   if Assigned(FOnShowList) then
    begin
     showlist:=true;
     FOnShowList(self,highoffs,s1,s2,showlist);
     if showlist then
      begin
       hght:=sendmessage(handle,lb_getitemheight,0,0);
       idx:=sendmessage(handle,lb_gettopindex,0,0);

       flistbox.top:=self.clienttoscreen(point(0,0)).y+hght*(highidx-idx+1);
       flistbox.left:=self.clienttoscreen(point(0,0)).x+w;
       flistbox.ownerlist:=self;
       flistbox.ParamIdx := highoffs;
       flistbox.visible:=true;

       if fAutoDropWidth then  flistbox.sizedropdownwidth else flistbox.width:=fDropWidth;
       flistbox.Height:=fDropHeight;

       flistbox.SetZOrder(true);
       flistbox.itemindex:=0;
       flistbox.Setfocus;

//       SetCapture(flistbox.handle);
      end;
    end;

   if not showlist and Assigned(FOnParameterClick) then
    begin
     FOnParameterClick(self,highoffs,s1,s2);
     self.setParam(highoffs,s2);
    end;

  end;

end;

procedure TParameterListBox.WMLButtonUp(var Msg:TWMLButtonDown);
var
 idx,hght,clk,l,wdth,lx:longint;
 s,s1,s2:string;
 ps1,ps2:string;
 param:tcontrolclickparameter;
 showlist:boolean;
 showcontrol:boolean;

begin
 inherited;

 idx:=sendmessage(self.handle,lb_gettopindex,0,0);
 hght:=sendmessage(self.handle,lb_getitemheight,0,0);
 wdth:=0;

 if flistbox.visible then exit;

 clk:=(msg.ypos div hght)+idx;

 if (clk<items.count) and (clk>=0) then
  begin
   s:=items[clk];
   if self.HasParam(s) then
    while self.HasParam(s) do
     begin
      s1:=self.GetFirstText(s);
      if self.GetParamValue(s)='' then
        s2:=s1+self.getParamName(s)
      else
        s2:=s1+self.getParamValue(s);

      if (msg.xpos>canvas.textwidth(s1)+wdth) and
         (msg.xpos<canvas.textwidth(s2)+wdth) then
        begin {clicked in right zone}
           lx:=canvas.textwidth(s1)+wdth;
           l:=self.getParamIdx(s);
           ps1:=self.getParamName(s);
           ps2:=self.getParamValue(s);

           showlist:=false;

           if Assigned(FOnShowList) then
            begin
             showlist:=true;
             FOnShowList(self,l,ps1,ps2,showlist);
             if showlist then
              begin
               flistbox.top:=self.clienttoscreen(point(0,0)).y+hght*(clk-idx+1);
               flistbox.left:=self.clienttoscreen(point(0,0)).x+lx;
               flistbox.ownerlist:=self;
               flistbox.ParamIdx := l;
               flistbox.visible:=true;
               if fAutoDropWidth then flistbox.sizedropdownwidth else flistbox.width:=fDropWidth;
               flistbox.height:=fDropHeight;
//               SetCapture(flistbox.handle);
               flistbox.SetZOrder(true);
               flistbox.itemindex:=0;
               flistbox.Setfocus;
              end;
            end;

           showcontrol:=false;

           if not showlist and Assigned(FOnControlClick) then
            begin
             showcontrol:=true;
             param.paramrect.top:=hght*(clk-idx);
             param.paramrect.left:=canvas.textwidth(s1);
             param.paramrect.bottom:=param.paramrect.top+hght;
             param.paramrect.right:=canvas.textwidth(s2);
             param.mousex:=msg.xpos;
             param.mousey:=msg.ypos;
             param.index:=self.getParamIdx(s);
             FOnControlClick(self,param,ps1,ps2,showcontrol);
            end;

           if not showcontrol and not showlist and Assigned(FOnParameterClick) then
            begin
             FOnParameterClick(self,l,ps1,ps2);
             self.setParam(l,ps2);
            end;
        end;

       s:=GetLastText(s);
       wdth:=wdth+canvas.textwidth(s2);
     end;
  end;
end;

procedure TParameterListBox.CMMouseLeave(var Message: TMessage);
begin
 if highlight then invalidate;
 highlight:=false;
end;

function TParameterListBox.GetActive: integer;
begin
 result:=HighOffs;
end;

procedure TParameterListBox.SetActive(value: integer);
var
  r: TRect;
begin
  HighIdx := GetParamPos(HighOffs);
  SendMessage(Handle,lb_getitemrect,highidx,LParam(@r));
  Invalidaterect(handle,@r,false);

  HighOffs := value;
  HighLight := (value > 0);
  HighIdx := GetParamPos(HighOffs);
  SendMessage(Handle,lb_getitemrect,highidx,LParam(@r));
  Invalidaterect(Handle,@r,false);
end;

procedure TParameterListBox.SetDelimiter(const Value: string);
begin
 fDelimiter := Value;
 FStartDelimiter:=FDelimiter[1];
 FEndDelimiter:=FDelimiter[2];
 self.repaint;
end;

procedure TParameterListBox.InitParams;
var
 i,j,k:integer;

begin
 i:=self.GetParamMin;
 j:=self.GetParamMax;
 for k:=i to j do
  begin
   Parameter[k]:='';
  end;
end;

procedure TParameterListBox.LoadParams;
var
 inifile:tinifile;
 {$IFDEF WIN32}
 reginifile:treginifile;
 {$ENDIF}
 i,j,k:integer;
begin
 if (fSaveParams.fApp='') or
    (fSaveParams.fKey='') or
    (fSaveParams.fSection='') then exit;

 i:=self.GetParamMin;
 j:=self.GetParamMax;

 if i<0 then exit;

 {$IFDEF WIN32}
 if fSaveParams.Location = lcRegistry then
  begin
   reginifile:=tRegInifile.Create(fSaveParams.fApp);
   with fSaveParams do
    for k:=i to j do
     begin
      Parameter[k]:=reginifile.readstring(fSection,fKey+inttostr(k),'')
     end;
   reginifile.Free;
  end
 else
  begin
 {$ENDIF}
   inifile:=tInifile.Create(fSaveParams.fApp);
   with fSaveParams do
    for k:=i to j do
     begin
      Parameter[k]:=inifile.readstring(fSection,fKey+inttostr(k),'');
     end;
   inifile.free;
 {$IFDEF WIN32}
  end;
 {$ENDIF}

end;

procedure TParameterListBox.SaveParams;
var
 inifile:tinifile;
 {$IFDEF WIN32}
 reginifile:treginifile;
 {$ENDIF}
 i,j,k:integer;
begin
 if (fSaveParams.fApp='') or
    (fSaveParams.fKey='') or
    (fSaveParams.fSection='') then exit;

 i:=self.GetParamMin;
 j:=self.GetParamMax;

 if i<0 then exit;
 {$IFDEF WIN32}
 if fSaveParams.Location = lcRegistry then
  begin
   reginifile:=tRegInifile.Create(fSaveParams.fApp);
   with fSaveParams do
     for k:=i to j do
      begin
       reginifile.writestring(fSection,fKey+inttostr(k),Parameter[k])
      end;
   reginifile.Free;
  end
 else
  begin
 {$ENDIF}
   inifile:=tInifile.Create(fSaveParams.fApp);
   with fSaveParams do
     for k:=i to j do
      begin
       inifile.writestring(fSection,fKey+inttostr(k),Parameter[k]);
      end;
   inifile.free;
 {$IFDEF WIN32}
  end;
 {$ENDIF} 
end;

function TParameterListBox.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TParameterListBox.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TParameterListBox.SetVersion(const Value: string);
begin

end;

{ TPopupListBox ; Helper listbox object}
procedure TPopupListBox.SizeDropDownWidth;
var
 i:integer;
 tw,nw:integer;
 lpmin,lpmax:integer;
 scrlw:integer;
begin
 GetScrollRange(self.Handle,SB_VERT,lpmin,lpmax);
 if (lpmin<>0) or (lpmax<>0) then
  begin
   scrlw:=GetSystemMetrics(SM_CXVSCROLL);
  end
 else scrlw:=0;
 tw:=0;
 if self.Items.Count>0 then tw:=0;
 for i:=1 to self.Items.Count do
  begin
   nw:=10+canvas.textwidth(self.items[i-1]); {account for border size?}
   if (nw>tw) then tw:=nw;
  end;
 self.width:=tw+scrlw;
end;

procedure TPopupListBox.CreateParams(var Params: TCreateParams);
begin
 inherited CreateParams(params);
 params.style:=params.style AND NOT (WS_CHILD) OR (WS_POPUP);
end;

procedure TPopupListBox.WMKeyDown(var Msg: TWMKeydown);
begin
 inherited;
 if msg.charcode = vk_return then
  begin
   if (self.itemindex>=0) then
     begin
     if assigned(fOwnerList.fOnComboSelect) then
      fOwnerList.fOnComboSelect(fOwnerList,paramidx,
                                           fOwnerList.Parameter[ParamIdx],
                                           self.Items[self.ItemIndex]);

     fOwnerList.Parameter[ParamIdx]:=self.Items[self.ItemIndex];

     end;
   self.parent.setfocus;
   self.visible:=false;
//   releasecapture;
  end;
 if msg.CharCode = vk_Escape then
   begin
    self.parent.setfocus;
    self.visible:=false;
//    releasecapture;
   end;

end;

procedure TPopupListBox.WMLButtonDown(var Message: TWMLButtonDown);
begin
 inherited;
end;


procedure TPopupListBox.WMLButtonUp(var Message: TWMLButtonDown);
begin
 inherited;
 if (self.itemindex>=0) then
  begin
   if assigned(fOwnerList.fOnComboSelect) then
    fOwnerList.fOnComboSelect(fOwnerList,paramidx,
                                         fOwnerList.Parameter[ParamIdx],
                                         self.Items[self.ItemIndex]);
   fOwnerList.Parameter[ParamIdx]:=self.Items[self.ItemIndex];
  end;
 self.parent.setfocus;
 self.visible:=false;
// releasecapture;
end;


{ TSaveParams }

constructor TSaveParams.Create(aOwner: TParameterListBox);
begin
 inherited Create;
 fOwner:=aOwner;
end;

destructor TSaveParams.Destroy;
begin
 inherited Destroy;
end;



begin
 {$IFDEF CURSORRESOURCE}
 Screen.Cursors[crHandCur] := LoadCursor(HInstance,PChar(8888));
 {$ENDIF}
end.

{********************************************************************}
{ TPICKDIALOG component                                              }
{ for Delphi & C++ Builder                                           }
{                                                                    }
{ written by                                                         }
{          TMS Software                                              }
{          copyright © 1998-2012                                     }
{          Email : info@tmssoftware.com                              }
{          Website : http://www.tmssoftware.com                      }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the author and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit PickDlg;

{$I TMSDEFS.INC}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 6; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.5.0.1 : Fixed issue to copy also objects to selectlist
  // v1.6.0.0 : New : Font, ListFont property added
  // v1.6.1.0 : New : Support for multiline title

type
  TButtonPosition = (bpBottom,bpRight,bpNone);

  TDialogPosition = (fposCenter,fposAbsolute,fposDefault);

  TClickItemEvent = procedure(Sender:TObject;index:integer;itemstr:string) of object;
  TDblClickItemEvent = procedure(Sender:TObject;index:integer;itemstr:string) of object;

  TPickDialog = class;

  TSelectForm = class(TForm)
    SelectList: TListBox;
    okbtn: TButton;
    cancelbtn: TButton;
    title: TLabel;
    procedure okbtnClick(Sender: TObject);
    procedure cancelbtnClick(Sender: TObject);
    procedure SelectListDblClick(Sender: TObject);
    procedure SelectListClick(Sender: TObject);
  private
    fparentcontrol:TPickDialog;
    { Private declarations }
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message wm_EraseBkGnd;
    procedure WMSize(var Message: TWMSize); message wm_size;
    procedure WMNCHitTest(var Message: TMessage); message wm_nchittest;
    procedure WMMinMaxInfo(var Message: TMessage); message wm_getminmaxinfo;

  public
    cresizex,cresizey:integer;
    fbuttonpos:tbuttonposition;
    acceptdblclick:boolean;
    { Public declarations }
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TPickDialog = class(TComponent)
  private
    fCaption:string;
    fTitle:string;
    fPickList:TStringlist;
    fSelectIndex:integer;
    fSelectString:string;
    fSelectList:tstringlist;
    fbuttonpos:tbuttonposition;
    fmultisel,fsort,fshowtitle:boolean;
    fDialogPosition:TDialogPosition;
    fDblClick:boolean;
    fHeight: integer;
    fWidth: integer;
    fTopPosition: integer;
    fLeftPosition: integer;
    fCancelCaption:string;
    fOkCaption:string;
    fSelectData:tObject;
    fSizeable:boolean;
    fToolWindow:boolean;
    fSelectForm:tSelectForm;
    fCount:integer;
    fOnClickItem:TClickItemEvent;
    fOnDblClickItem:TDblClickItemEvent;
    FListFont: TFont;
    FFont: TFont;
    procedure SetPickList(value: tstringlist);
    procedure SetHeight(value:integer);
    procedure SetWidth(value:integer);
    procedure CreateSelect;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetFont(const Value: TFont);
    procedure SetListFont(const Value: TFont);
  protected
    function GetVersionNr: Integer; virtual;
  public
    constructor Create(aOwner:TComponent); override;
    destructor Destroy; override;
    function Execute:integer;
    procedure Show;
    procedure Hide;
    property SelectIndex:integer read FSelectIndex write fSelectIndex;
    property SelectString:string read FSelectString;
    property SelectData:TObject read FSelectData;
    property SelectList:TStringList read FSelectList;
  published
    property PickItems:tStringlist read fPickList write SetPickList;
    property Caption:string read fCaption write fCaption;
    property Title:string read fTitle write fTitle;
    property MultiSel:boolean read fMultisel write fMultisel;
    property AcceptDblClick: Boolean read fDblClick write fDblClick;
    property Sort: Boolean read fSort write fSort;
    property ShowTitle: Boolean read fShowTitle write fShowTitle;
    property ButtonPosition: TButtonPosition read fButtonpos write fButtonpos;
    property Width:integer read fWidth write SetWidth;
    property Height:integer read fHeight write SetHeight;
    property TopPosition:integer read fTopPosition write fTopPosition;
    property LeftPosition:integer read fLeftPosition write fLeftPosition;
    property DialogPosition:TDialogPosition read fDialogPosition write fDialogPosition;
    property CancelCaption:string read fCancelCaption write fCancelCaption;
    property OkCaption:string read fOkCaption write fOkCaption;
    property Sizeable:boolean read fSizeAble write fSizeAble;
    property ToolWindow:boolean read fToolWindow write fToolWindow;
    property OnClickItem:TClickItemEvent read FOnClickItem write FOnClickItem;
    property OnDblClickItem:TDblClickItemEvent read FOnDblClickItem write FOnDblClickItem;
    property Version: string read GetVersion write SetVersion;
    property Font: TFont read FFont write SetFont;
    property ListFont: TFont read FListFont write SetListFont;
  end;


implementation

{$R *.DFM}

{ TPickDialog }

constructor TPickDialog.Create(aOwner: tComponent);
begin
  inherited Create(aOwner);
  FPickList := TStringlist.Create;
  FSelectList := TStringlist.Create;
  FFont := TFont.Create;
  FListFont := TFont.Create;
  FSelectIndex:=-1;
  FSelectString:='';
  FWidth:=280;
  FHeight:= 270;
  FCancelCaption:='Cancel';
  FOkCaption:='OK';
  FCount:=0;
end;

procedure TPickDialog.SetPickList(value: tstringlist);
begin
  if Assigned(value) then
    fpicklist.Assign(value);
end;

procedure TPickDialog.CreateSelect;
var
  r:trect;
  {
  i:integer;
  clientheight:integer;
  clientwidth:integer;
  }
begin
  if FCount > 0 then
    Exit;

  if (csDesigning in ComponentState) then
    FSelectForm := TSelectForm.Create(Application)
  else
    FSelectForm := TSelectForm.Create(Owner);

  FSelectForm.Width:=fwidth;
  FSelectForm.Height:=fHeight;
  FSelectForm.CreSizeX:=fWidth;
  FSelectForm.CreSizeY:=fHeight;
  FSelectForm.acceptdblclick :=fDblClick;
  FSelectForm.Fbuttonpos := fbuttonpos;
  FSelectForm.FParentControl:=self;

  case fDialogPosition of
  fposCenter:fSelectForm.Position:=poScreenCenter;
  fposDefault:fSelectForm.Position:=poDefaultPosOnly;
  fposAbsolute:
  begin
    fSelectForm.Top:=fTopPosition;
    fSelectForm.Left:=fLeftPosition;
    fSelectForm.Position:=poDesigned;
  end;
  end;

  if FToolwindow then
  begin
    if FSizeable then
      FSelectForm.BorderStyle := bsSizeToolWin
    else
      FSelectForm.BorderStyle := bsToolWindow;
  end
 else
  begin
    if FSizeable then
      FSelectForm.BorderStyle := bsSizeable
    else
      FSelectForm.BorderStyle := bsSingle;
  end;

  r := FSelectform.GetClientRect;
  FSelectForm.Font.Assign(FFont);
 {
 clientheight:=r.bottom-r.top;
 clientwidth:=r.right-r.left;
 }
  with FSelectForm do
  begin
    SelectList.Items.AddStrings(fPickList);
    Caption := FCaption;
    Okbtn.caption := fOkCaption;
    Cancelbtn.caption := fCancelCaption;
    Title.Caption := FTitle;
    Title.AutoSize := true;
    SelectList.MultiSelect := FMultisel;
    SelectList.Sorted := FSort;
    SelectList.Itemindex := 1;
    SelectList.Font.Assign(FListFont);

    if (SelectList.Items.Count > 0) then
    begin
      if (fSelectIndex = -1) then
        SelectList.ItemIndex := 0
      else
        SelectList.ItemIndex := FSelectIndex;
    end;

    if not FShowTitle then
    begin
      Title.Visible := false;
      SelectList.top := SelectList.top-title.height-4;
      SelectList.Height:=SelectList.Height+title.height+4;
      SelectList.left:=8;
     end
     else
       SelectList.Top := Title.Top + Title.Height + 2;


   case fbuttonpos of
    bpNone:begin
            SelectList.Width :=ClientWidth - 2*8;
            SelectList.Height:=Clientheight- SelectList.top-2*8;
            cancelbtn.Visible:=false;
            okbtn.visible:=false;
           end;
    bpBottom:
       begin
         SelectList.Width :=ClientWidth - 2*8;
         SelectList.Height:=Clientheight- SelectList.top-2*8-cancelbtn.height;

         cancelbtn.top:=ClientHeight-cancelbtn.height-8;
         cancelbtn.left:=ClientWidth-cancelbtn.width-8;
         okbtn.top:=clientheight-8-okbtn.height;
         okbtn.left:=ClientWidth-2*cancelbtn.width-2*8;
       end;
    bpRight:
       begin
         SelectList.Width :=ClientWidth - cancelbtn.width- 3*8;
         SelectList.Height:=ClientHeight- SelectList.top-8;
         cancelbtn.top:=ClientHeight-2*cancelbtn.height-2*8;
         cancelbtn.left:=ClientWidth-cancelbtn.width-8;
         okbtn.top:=ClientHeight-8-okbtn.height;
         okbtn.left:=cancelbtn.left;
       end;
    end;
  end;
 inc(fcount);
end;

procedure TPickDialog.Show;
begin
  CreateSelect;
  FSelectForm.FormStyle:=fsStayontop;
  FSelectForm.Show;
end;

procedure TPickDialog.Hide;
begin
  if (fcount > 0) then
    FSelectForm.Free;
  FCount := 0;
end;

function TPickDialog.Execute:integer;
var
  i: Integer;
begin
  try
    CreateSelect;

    with FSelectForm do
    begin
      Result := ShowModal;
      FSelectIndex := SelectList.ItemIndex;
      if (FSelectIndex >= 0) and (Result = mrOK) then
      begin
        FSelectString := SelectList.Items[FSelectIndex];
        FSelectData := TObject(sendmessage(selectlist.handle,lb_getitemdata,FSelectIndex,0));
      end;
      if FMultiSel and (SelectList.items.count > 0) and (Result = mrOK) then
      begin
        FSelectList.Clear;
        for i := 0 to SelectList.Items.Count - 1 do
        begin
          if SelectList.Selected[i] then
          begin
            FSelectList.AddObject(SelectList.Items[i], SelectList.Items.Objects[i]);
          end;
        end;
      end;
    end;
  finally
    FSelectForm.Free;
    FCount := 0;
  end;
end;

destructor TPickDialog.Destroy;
begin
  FSelectList.Free;
  FPickList.Free;
  FFont.Free;
  FListFont.Free;
  inherited Destroy;
end;

procedure TSelectForm.okbtnClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TSelectForm.cancelbtnClick(Sender: TObject);
begin
  Modalresult := mrCancel;
end;

procedure TPickDialog.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TPickDialog.SetHeight(value: integer);
begin
  if value > 180 then
    FHeight := value;
end;

procedure TPickDialog.SetListFont(const Value: TFont);
begin
  FListFont.Assign(Value);
end;

procedure TPickDialog.SetWidth(value: integer);
begin
  if value > 180 then
    FWidth := value;
end;

function TPickDialog.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TPickDialog.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TPickDialog.SetVersion(const Value: string);
begin

end;

procedure TSelectForm.SelectListDblClick(Sender: TObject);
begin
 if acceptdblclick then modalresult:=mrOk;

 with (fParentControl as TPickDialog) do
  if assigned(OnDblClickItem) then
   fOnDblClickItem(owner,self.selectlist.itemindex,
      self.selectlist.items[self.selectlist.itemindex]);

end;

procedure TSelectForm.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
 r:trect;
begin
 inherited;
 if (BorderStyle=bsSingle) then exit;
 r:=clientrect;
 r.left:=r.right-GetSystemMetrics(SM_CXVSCROLL);
 r.top:=r.bottom-GetSystemMetrics(SM_CXHSCROLL);
 DrawFrameControl(canvas.handle,r,DFC_SCROLL,DFCS_SCROLLSIZEGRIP);
end;

procedure TSelectForm.WMNCHitTest(var Message: TMessage);
var
 r:trect;
 p:tpoint;
begin
 if (BorderStyle=bsSingle) then
   inherited
 else
  begin
   r:=clientrect;
   r.left:=r.right-GetSystemMetrics(SM_CXVSCROLL);
   r.top:=r.bottom-GetSystemMetrics(SM_CXHSCROLL);

   p.x:=loword(message.lparam);
   p.y:=hiword(message.lparam);
   p:=screentoclient(p);

   if ptInRect(r,p) then
      message.result:=HTBOTTOMRIGHT

   else inherited;
  end;
end;

procedure TSelectForm.WMMinMaxInfo(var Message: TMessage);
begin
 with PMinMaxInfo(message.lParam)^ do
 begin
  ptMinTrackSize.X := CreSizeX;
  ptMinTrackSize.Y := CreSizeY;
 end;
end;

procedure TSelectForm.WMSize(var Message: TWMSize);
begin
 if (BorderStyle in [bsSizeable,bsSizeToolWin]) then
  invalidaterect(self.handle,nil,true);
 inherited;
 {resize the list here}
 if (BorderStyle in [bsSizeable,bsSizeToolWin]) then
  begin
    case fbuttonpos of
    bpNone:begin
            SelectList.Width :=ClientWidth - 2*8;
            SelectList.Height:=Clientheight- SelectList.top-2*8;
            cancelbtn.visible:=false;
            okbtn.visible:=false;
           end;
    bpBottom:
       begin
         SelectList.Width :=ClientWidth - 2*8;
         SelectList.Height:=Clientheight- SelectList.top-2*8-cancelbtn.height;
         cancelbtn.top:=ClientHeight-cancelbtn.height-8;
         cancelbtn.left:=ClientWidth-cancelbtn.width-8;
         okbtn.top:=clientheight-8-okbtn.height;
         okbtn.left:=ClientWidth-2*cancelbtn.width-2*8;
       end;
    bpRight:
       begin
         SelectList.Width :=ClientWidth - cancelbtn.width- 3*8;
         SelectList.Height:=ClientHeight- SelectList.top-8;
         cancelbtn.top:=ClientHeight-2*cancelbtn.height-2*8;
         cancelbtn.left:=ClientWidth-cancelbtn.width-8;
         okbtn.top:=ClientHeight-8-okbtn.height;
         okbtn.left:=cancelbtn.left;
       end;
    end;
  end;
end;

procedure TSelectForm.SelectListClick(Sender: TObject);
begin
 {do some code here too}
 with (fParentControl as TPickDialog) do
  if assigned(OnClickItem) then
   fOnClickItem(owner,self.selectlist.itemindex,
      self.selectlist.items[self.selectlist.itemindex]);
end;

end.

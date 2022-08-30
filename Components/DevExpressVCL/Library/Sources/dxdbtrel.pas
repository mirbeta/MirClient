{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express data-aware tree view edit controls               }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE and ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL and PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSGRID and ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE and ALL RELATED       }
{   FILES or ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, or OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   and PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxdbtrel;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  DB, StdCtrls, ExtCtrls, ComCtrls, Menus, Variants, Types, ImgList,
  dxCore, dxCommon, cxControls, cxClasses, cxGeometry, cxGraphics,
  dxdbtree, dxtree, DBCtrls;

type
  TTVTextStyle = (tvtsShort, tvtsFull);

  TdxTreeViewCloseUp = procedure (Sender: TObject; Accept: Boolean) of object;

  TCustomdxVTreeViewEdit = class(TCustomControl)
  private
    FCanSelectParents: Boolean;
    FAlignment: TAlignment;
    FFocused: Boolean;
    FButtonWidth: Integer;
    FDividedChar: Char;
    FDropDownRows: Integer;
    FListVisible: Boolean;
    FOnDropDown: TNotifyEvent;
    FOnCloseUp: TdxTreeViewCloseUp;
    FText: string;
    FTextStyle: TTVTextStyle;
    FOnGetSelectedIndex: TTVExpandedEvent;
    FOnGetImageIndex: TTVExpandedEvent;
    FDropDownWidth: Integer;
    FPressed: Boolean;
    FOldParentForm: TCustomForm;

    procedure SetDividedChar(Value: Char);
    procedure SetText(Value: string);

    function CanSelectTreeNode(ANode: TTreeNode): Boolean;
    function GetCustomDraw: TTreeViewCustomDraw;
    function GetTreeViewColor: TColor;
    function GetTreeViewCursor: TCursor;
    function GetTreeViewFont: TFont;
    function GetTreeViewHint: string;
    function GetTreeViewImages: TCustomImageList;
    function GetTreeViewIndent: Integer;
    function GetTreeViewPopupMenu: TPopupMenu;
    function GetTreeViewReadOnly: Boolean;
    function GetSelectedTreeNode: TTreeNode;
    function GetTreeViewShowButtons: Boolean;
    function GetTreeViewShowHint: Boolean;
    function GetTreeViewShowLines: Boolean;
    function GetTreeViewShowRoot: Boolean;
    function GetTreeViewSortType: TSortType;
    function GetTreeViewStateImages: TCustomImageList;

    procedure SetCustomDraw(Value: TTreeViewCustomDraw);
    procedure SetTreeViewColor(Value: TColor);
    procedure SetTreeViewCursor(Value: TCursor);
    procedure SetTreeViewFont(Value: TFont);
    procedure SetTreeViewHint(Value: string);
    procedure SetTreeViewImages(Value: TCustomImageList);
    procedure SetTreeViewIndent(Value: Integer);
    procedure SetTreeViewPopupMenu(Value: TPopupMenu);
    procedure SetTreeViewReadOnly(Value: Boolean);
    procedure SetTreeViewShowButtons(Value: Boolean);
    procedure SetTreeViewShowHint(Value: Boolean);
    procedure SetTreeViewShowLines(Value: Boolean);
    procedure SetTreeViewShowRoot(Value: Boolean);
    procedure SetTreeViewSortType(Value: TSortType);
    procedure SetTreeViewStateImages(Value: TCustomImageList);

    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;

    function GetBorderSize: Integer;
    function GetTextHeight: Integer;
    procedure AllowChangeTreeNode(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    procedure ClickTreeView(Sender: TObject);
    procedure DblClickTreeView(Sender: TObject);
  protected
    function VirtualTreeView: TCustomdxTreeView; virtual; abstract;

    function IsReadOnly: Boolean; virtual;

    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure DropDownOk; virtual;
    procedure CreateVirtualTreeView; virtual;
    procedure PrepareDropDown; virtual;
    function GetPaintedText: string; virtual;

    property Alignment: TAlignment read FAlignment write FAlignment;
    property DividedChar: Char read FDividedChar write SetDividedChar;
    property TextStyle: TTVTextStyle read FTextStyle  write FTextStyle;

    property Selected: TTreeNode read GetSelectedTreeNode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure CloseUp(Accept: Boolean); virtual;
    procedure DropDown; virtual;
  published
    property CanSelectParents: Boolean read FCanSelectParents write FCanSelectParents;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property DropDownRows: Integer read FDropDownRows write FDropDownRows default 7;
    property DropDownWidth: Integer read FDropDownWidth write FDropDownWidth default 0;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text: string read FText write SetText;
    property TreeViewColor: TColor read GetTreeViewColor write SetTreeViewColor;
    property TreeViewCursor: TCursor read GetTreeViewCursor write SetTreeViewCursor;
    property TreeViewFont: TFont read GetTreeViewFont write SetTreeViewFont;
    property TreeViewHint: string read GetTreeViewHint write SetTreeViewHint;
    property TreeViewImages: TCustomImageList read GetTreeViewImages write SetTreeViewImages;
    property TreeViewIndent: Integer read GetTreeViewIndent write SetTreeViewIndent;
    property TreeViewPopupMenu: TPopupMenu read GetTreeViewPopupMenu write SetTreeViewPopupMenu;
    property TreeViewReadOnly: Boolean read GetTreeViewReadOnly write SetTreeViewReadOnly;
    property TreeViewShowButtons: Boolean read GetTreeViewShowButtons write SetTreeViewShowButtons;
    property TreeViewShowHint: Boolean read GetTreeViewShowHint write SetTreeViewShowHint;
    property TreeViewShowLines: Boolean read GetTreeViewShowLines write SetTreeViewShowLines;
    property TreeViewShowRoot: Boolean read GetTreeViewShowRoot write SetTreeViewShowRoot;
    property TreeViewSortType: TSortType read GetTreeViewSortType write SetTreeViewSortType;
    property TreeViewStateImages: TCustomImageList read GetTreeViewStateImages write SetTreeViewStateImages;
    property Visible;
    property OnCloseUp: TdxTreeViewCloseUp read FOnCloseUp write FOnCloseUp;
    property OnClick;
    property OnCustomDraw: TTreeViewCustomDraw read GetCustomDraw write SetCustomDraw;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSelectedIndex: TTVExpandedEvent read FOnGetSelectedIndex write FOnGetSelectedIndex;
    property OnGetImageIndex: TTVExpandedEvent read FOnGetImageIndex write FOnGetImageIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;

    property Anchors;
    property Constraints;
    property OnStartDock;
    property OnEndDock;
  end;

  TCustomdxTreeViewEdit = class(TCustomdxVTreeViewEdit)
  private
    FMemStream: TMemoryStream;
    FImagesStream: TMemoryStream;
    FOldSelected: TTreeNode;
    FTreeView: TdxTreeView;

    function GetItems: TTreeNodes;
    procedure SetItems(Value: TTreeNodes);
    procedure SaveNodesToStream;
  protected
    function VirtualTreeView: TCustomdxTreeView; override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure DestroyWindowHandle; override;

    procedure CreateHandle; override;
    procedure DestroyHandle; override;

    procedure PrepareDropDown; override;
    procedure SearchTreeNode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CloseUp(Accept: Boolean); override;
    procedure DropDown; override;
    function GetTreeNodeByText(ParentTreeNode: TTreeNode; Text: string; flag: Boolean): TTreeNode;

    property Selected;
  published
    property DividedChar;
    property Items: TTreeNodes read GetItems write SetItems;
    property TextStyle;
  end;

  TdxTreeViewEdit = class(TCustomdxTreeViewEdit)
  published
    property Alignment;
  end;

  TdxDBTreeViewEdit = class(TCustomdxTreeViewEdit)
  private
    FDataLink: TFieldDataLink;
    FCanvas: TControlCanvas;

    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure DropDownOk; override;
    function GetPaintedText: string; override;

    function IsReadOnly: Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DropDown; override;

    property Field: TField read GetField;
    property Text;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;

  TCustomdxLookupTreeView = class;

  TDataLinkLookupTreeView = class(TDataLink)
  private
    LookupTreeView: TCustomdxLookupTreeView;
  protected
    procedure ActiveChanged; override;
  end;

  TCustomdxLookupTreeView = class(TCustomdxVTreeViewEdit)
  private
    DBTreeView: TdxDBTreeView;
    ListLink: TDataLinkLookupTreeView;
    FKeyFieldValue: Variant;
    FAssignFieldName: string;
    FKeyFieldName: string;
    FParentFieldName: string;
    FListFieldName: string;
    FDisplayFieldName: string;
    FImageIndexFieldName: string;
    FStateIndexFieldName: string;
    FDropDownKeyValue: Variant;

    FOnSetDisplayItemText: TSetDisplayItemText;

    function GetAddNewItem: TAddNewDBTreeNodeEvent;
    function GetListSource: TDataSource;
    function GetOptions: TdxDBTreeViewOptions;
    function GetRootValue: Variant;
    procedure SetAddNewItem(Value: TAddNewDBTreeNodeEvent);
    procedure SetAssignField(Value: string);
    procedure SetKeyField(Value: string);
    procedure SetListField(Value: string);
    procedure SetOptions(Value: TdxDBTreeViewOptions);
    procedure SetParentField(Value: string);
    procedure SetRootValue(Value: Variant);
    procedure SetListSource(Value: TDataSource);
  protected
    FAssignField: TField;
    FKeyField: TField;
    FListField: TField;
    FParentField: TField;

    function VirtualTreeView: TCustomdxTreeView; override;
    procedure DropDownOk; override;
    procedure DataLinkActiveChanged; virtual;
    procedure ResetDropDown; virtual;

    property AssignField: string read FAssignFieldName write SetAssignField;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DropDown; override;
    procedure CloseUp(Accept: Boolean); override;
  published
    property DisplayField: string read FDisplayFieldName write FDisplayFieldName;
    property DividedChar;
    property ImageIndexField: string read FImageIndexFieldName write FImageIndexFieldName;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property KeyField: string read FKeyFieldName write SetKeyField;
    property ListField: string read FListFieldName write SetListField;
    property Options: TdxDBTreeViewOptions read GetOptions write SetOptions default [];
    property ParentField: string read FParentFieldName write SetParentField;
    property RootValue: Variant read GetRootValue write SetRootValue;
    property StateIndexField: string read FStateIndexFieldName write FStateIndexFieldName;
    property TextStyle;
    property OnAddNewItem: TAddNewDBTreeNodeEvent read GetAddNewItem write SetAddNewItem;
    property OnSetDisplayItemText: TSetDisplayItemText read FOnSetDisplayItemText
                                 write FOnSetDisplayItemText;
  end;

  TdxLookupTreeView = class(TCustomdxLookupTreeView)
  published
    property Alignment;
  end;

  TdxDBLookupTreeView = class(TCustomdxLookupTreeView)
  private
    FDataLink: TFieldDataLink;
    FCanvas: TControlCanvas;
    FCloseUpFlag: Boolean;
    FUpdateDataFlag: Boolean;

    procedure ActiveChange(Sender: TObject);
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;

    procedure CheckNotCircular;
  protected
    procedure DataLinkActiveChanged; override;
    procedure DropDownOk; override;
    function GetPaintedText: string; override;
    function IsReadOnly: Boolean; override;
    function GetDisplayText: string;
    function GetLookupValue: Variant;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure PrepareDropDown; override;
    procedure SearchTreeNode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DropDown; override;
    procedure CloseUp(Accept: Boolean); override;

    property Field: TField read GetField;
    property Text;
  published
    property AssignField;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;

implementation

uses
  CommCtrl, DBConsts, cxLookAndFeelPainters, dxDPIAwareUtils;

const
  TreeBorderHeight = 19;

function DBTrDataSetLocate(DataSet: TDataSet; const AFieldName: string; AValue: Variant; AOptions: TLocateOptions): Boolean;
begin
  Result := DataSet.Locate(AFieldName, AValue, AOptions);
end;

function VarEquals(const V1, V2: Variant): Boolean;
begin
  Result := False;
  try
    Result := V1 = V2;
  except
  end;
end;

function GetFullTreeNodeName(TreeNode: TTreeNode; DividedChar: Char): string;
var
  tr: TTreeNode;
begin
  Result := '';
  tr := TreeNode;
  while tr <> nil do begin
    if (Length(Result) > 0 ) and (Length(tr.Text) > 0 ) then
      Result := DividedChar + Result;
    Result := tr.Text + Result;
    tr := tr.Parent;
  end;
end;

function GetRealParentForm(AControl: TWinControl): TCustomForm;
var
  AForm: TCustomForm;
begin
  Result := GetParentForm(AControl);
  if Result <> nil then
  begin
    AForm := GetParentForm(Result);
    while (AForm <> nil) and (AForm <> Result) do
    begin
      Result := AForm;
      AForm := GetParentForm(AForm);
    end;
  end;
end;

function GetParentWinControl(AControl: TWinControl): TWinControl;
begin
  Result := GetParentForm(AControl);
  if Result = nil then
  begin
    Result := AControl;
    while Result.Parent <> nil do
      Result := Result.Parent;
  end;
end;

{TPopupTreeView}
type
  TCustomdxTreeViewAccess = class(TCustomdxTreeView);

  TPopupTreeViewHelper = class
  private
    FTreeView: TCustomdxTreeView;
    FTreeViewEdit: TCustomdxVTreeViewEdit;

    FHScrollWidth: Integer;
    FVScrollWidth: Integer;
    FCloseButtonRect: TRect;
    FGripRect: TRect;
    FCloseButtonIsTracking: Boolean;
    FMouseAboveCloseButton: Boolean;
    FCorner: TdxCorner;
  private
    procedure WMActivate(var Message: TWMActivate);
    procedure WMGetDlgCode(var Message: TWMGetDlgCode);
    procedure WMMouseActivate(var Message: TWMMouseActivate);
    procedure WMNCCalcSize(var Message: TWMNCCalcSize);
    procedure WMNCHitTest(var Message: TWMNCHitTest);
    procedure WMNCPaint(var Message: TWMNCPaint);
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
    procedure WMCaptureChanged(var Message: TMessage);
    procedure WMLButtonUp(var Message: TWMLButtonUp);
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown);
    procedure CMMouseLeave(var Message: TMessage);

    procedure CreateParams(var Params: TCreateParams);
    procedure CreateWnd;
    procedure UpdateSizeGripCorner;
  public
    constructor Create(ATreeView: TCustomdxTreeView);
  end;

  TPopupTreeView = class(TdxTreeView, IdxPopupControl)
  private
    FHelper: TPopupTreeViewHelper;
    FOwnerControl: TWinControl;

    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;

    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;

    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure UpdateSizeGripCorner; override;
    // IdxPopupControl
    function GetOwnerControl: TWinControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WndProc(var Message: TMessage); override;
  end;

  TPopupDBTreeView = class(TdxDBTreeView, IdxPopupControl)
  private
    FHelper: TPopupTreeViewHelper;
    FOwnerControl: TWinControl;

    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;

    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;

    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure UpdateSizeGripCorner; override;
    // IdxPopupControl
    function GetOwnerControl: TWinControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WndProc(var Message: TMessage); override;
  end;

{ TdxDBTreeViewEdit }

constructor TdxDBTreeViewEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];

  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TdxDBTreeViewEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  FCanvas.Free;
  inherited Destroy;
end;

procedure TdxDBTreeViewEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TdxDBTreeViewEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    if FAlignment <> FDataLink.Field.Alignment then
      FAlignment := FDataLink.Field.Alignment;
    Text := FDataLink.Field.Text
  end
  else
    if csDesigning in ComponentState then
      Text := Name
    else
      Text := '';
end;

procedure TdxDBTreeViewEdit.UpdateData(Sender: TObject);
begin
  if (FDataLink.Field <> nil) and FDataLink.Edit then
    FDataLink.Field.Text := Text
  else
    Text := '';
end;

procedure TdxDBTreeViewEdit.DropDown;
begin
  (VirtualTreeView as TPopupTreeView).FOwnerControl := Self;
  inherited DropDown;
  FDataLink.Modified;
end;

procedure TdxDBTreeViewEdit.DropDownOk;
var
  AStoredText: string;
begin
  inherited DropDownOk;
  AStoredText := FText;
  if (FDataLink.Field <> nil) and (FDataLink.Edit) then
    FDataLink.Field.Text := AStoredText
  else Text := '';
end;

function TdxDBTreeViewEdit.GetPaintedText: string;
begin
  if FDataLink.Field <> nil then
    Result := FDataLink.Field.Text
  else Result := '';
end;

function TdxDBTreeViewEdit.IsReadOnly: Boolean;
begin
  Result := ReadOnly;
end;

function TdxDBTreeViewEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TdxDBTreeViewEdit.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

function TdxDBTreeViewEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TdxDBTreeViewEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TdxDBTreeViewEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TdxDBTreeViewEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TdxDBTreeViewEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TdxDBTreeViewEdit.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TdxDBTreeViewEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(FDataLink);
end;

{ TDataLinkLookupTreeView }

procedure TDataLinkLookupTreeView.ActiveChanged;
begin
  if LookupTreeView <> nil then
    LookupTreeView.DataLinkActiveChanged;
end;

{TdxTreeViewEdit}

constructor TCustomdxTreeViewEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTreeView := TPopupTreeView.Create(Self);
  CreateVirtualTreeView;
end;

destructor TCustomdxTreeViewEdit.Destroy;
begin
  FreeAndNil(FMemStream);
  FreeAndNil(FImagesStream);
  inherited Destroy;
end;

function TCustomdxTreeViewEdit.VirtualTreeView: TCustomdxTreeView;
begin
  Result := FTreeView;
end;

procedure TCustomdxTreeViewEdit.CreateWnd;
var
  ANode: TTreeNode;
  Index: Integer;
begin
  inherited CreateWnd;
  if FMemStream <> nil then
  begin
    if VirtualTreeView <> nil then
    begin
      VirtualTreeView.LoadFromStream(FMemStream);
      ANode := VirtualTreeView.Items[0];
      while ANode <> nil do
      begin
        FImagesStream.Read(Index, SizeOf(Integer));
        ANode.ImageIndex := Index;
        FImagesStream.Read(Index, SizeOf(Integer));
        ANode.SelectedIndex := Index;
        FImagesStream.Read(Index, SizeOf(Integer));
        ANode.StateIndex := Index;
        ANode := ANode.GetNext;
      end;
    end;
    FreeAndNil(FMemStream);
    FreeAndNil(FImagesStream);
  end;
end;

procedure TCustomdxTreeViewEdit.DestroyWnd;
begin
  SaveNodesToStream;
  inherited DestroyWnd;
end;

procedure TCustomdxTreeViewEdit.DestroyWindowHandle;
begin
  SaveNodesToStream;
  inherited DestroyWindowHandle;
end;

procedure TCustomdxTreeViewEdit.CreateHandle;
begin
  inherited;
  if csRecreating in VirtualTreeView.ControlState then
    TPopupTreeView(VirtualTreeView).UpdateRecreatingFlag(False);
end;

procedure TCustomdxTreeViewEdit.DestroyHandle;
var
  APopupTreeView: TPopupTreeView;
begin
  if csRecreating in ControlState then
  begin
    APopupTreeView := TPopupTreeView(VirtualTreeView);
    APopupTreeView.UpdateRecreatingFlag(True);
    APopupTreeView.DestroyHandle;
  end;
  inherited;
end;

procedure TCustomdxTreeViewEdit.SaveNodesToStream;
var
  ANode: TTreeNode;
begin
  if not cxIsDestroying(Self, True) and (VirtualTreeView <> nil) and (VirtualTreeView.Items.Count > 0) and (FMemStream = nil) then
  begin
    FMemStream := TMemoryStream.Create;
    VirtualTreeView.SaveToStream(FMemStream);
    FMemStream.Position := 0;
    FImagesStream := TMemoryStream.Create;
    ANode := VirtualTreeView.Items[0];
    while ANode <> nil do
    begin
      FImagesStream.Write(ANode.ImageIndex, SizeOf(Integer));
      FImagesStream.Write(ANode.SelectedIndex, SizeOf(Integer));
      FImagesStream.Write(ANode.StateIndex, SizeOf(Integer));
      ANode := ANode.GetNext;
    end;
    FImagesStream.Position := 0;
  end;
end;

procedure TCustomdxTreeViewEdit.PrepareDropDown;
begin
  inherited;
  SearchTreeNode;
end;

procedure TCustomdxTreeViewEdit.DropDown;
begin
  if GetRealParentForm(Self) <> FOldParentForm then
  begin
    FOldParentForm := GetRealParentForm(Self);
    TPopupTreeView(VirtualTreeView).RecreateWnd;
  end;
  (VirtualTreeView as TPopupTreeView).FOwnerControl := Self;
  FOldSelected := Selected;
  inherited DropDown;
end;

procedure TCustomdxTreeViewEdit.CloseUp(Accept: Boolean);
begin
  if FListVisible and not Accept and (VirtualTreeView <> nil) then
    VirtualTreeView.Selected := FOldSelected;
  inherited CloseUp(Accept);
end;

procedure TCustomdxTreeViewEdit.SearchTreeNode;
var
  i: Integer;
  St: string;
begin
  if Length(FText) = 0 then
    Exit;
  if TextStyle = tvtsShort then
  begin
    for i := 0 to VirtualTreeView.Items.Count - 1 do
      if FText = VirtualTreeView.Items[i].Text then
      begin
        VirtualTreeView.Selected := VirtualTreeView.Items[i];
        VirtualTreeView.Selected.MakeVisible;
        Break;
      end;
  end
  else
    for i := 0 to VirtualTreeView.Items.Count - 1 do
    begin
      St := GetFullTreeNodeName(VirtualTreeView.Items[i], FDividedChar);
      if FText = St then
      begin
        VirtualTreeView.Selected := VirtualTreeView.Items[i];
        VirtualTreeView.Selected.MakeVisible;
        Break;
      end;
    end;
end;

function TCustomdxTreeViewEdit.GetTreeNodeByText(ParentTreeNode: TTreeNode;
  Text: string; flag: Boolean): TTreeNode;
var
  tmp: TTreeNode;
begin
  Result := nil;
  if ParentTreeNode = nil then
  begin
    tmp := VirtualTreeView.Items.GetFirstNode;
    while (tmp <> nil) and (Result = nil) do
      if tmp.Text = Text then
        Result := tmp
      else
        tmp :=  tmp.GetNext;
  end
  else
  begin
    tmp := ParentTreeNode.GetFirstChild;
    while (tmp <> nil) and (Result = nil) do
      if tmp.Text = Text then
        Result := tmp
      else
      begin
        if flag and tmp.HasChildren then
          Result := GetTreeNodeByText(tmp, Text, flag);
        if Result = nil then
          tmp := ParentTreeNode.GetNextChild(tmp);
      end;
  end;
end;

function TCustomdxTreeViewEdit.GetItems: TTreeNodes;
begin
  Result := VirtualTreeView.Items;
end;

procedure TCustomdxTreeViewEdit.SetItems(Value: TTreeNodes);
begin
  VirtualTreeView.Items := Value;
end;

{TCustomdxTreeViewEdit}

constructor TCustomdxVTreeViewEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csOpaque, csReplicatable];
  if not NewStyleControls then
    ControlStyle := ControlStyle + [csFramed];
  ParentColor := False;
  TabStop := True;
  FCanSelectParents := True;

  Width := 145;
  Height := 0;
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
  FDropDownWidth := 0;
  FDividedChar := '.';
  FDropDownRows := 7;
  FOldParentForm := GetRealParentForm(Self);
end;

procedure TCustomdxVTreeViewEdit.CreateVirtualTreeView;
begin
  if VirtualTreeView <> nil then
  begin
    VirtualTreeView.Visible := False;
    VirtualTreeView.ShowNodeHint := False;
    VirtualTreeView.OnChanging := AllowChangeTreeNode;
    VirtualTreeView.OnDblClick := DblClickTreeView;
    VirtualTreeView.OnClick := ClickTreeView;
   end;
end;

destructor TCustomdxVTreeViewEdit.Destroy;
begin
  VirtualTreeView.Free;
  inherited;
end;

function TCustomdxVTreeViewEdit.GetBorderSize: Integer;
var
  WFA7E0C3F8D24C0D8814705C6F7A16DE: TCreateParams;
  R: TRect;
begin
  CreateParams(WFA7E0C3F8D24C0D8814705C6F7A16DE);
  SetRect(R, 0, 0, 0, 0);
  AdjustWindowRectEx(R, WFA7E0C3F8D24C0D8814705C6F7A16DE.Style, False, WFA7E0C3F8D24C0D8814705C6F7A16DE.ExStyle);
  Result := R.Bottom - R.Top;
end;

procedure TCustomdxVTreeViewEdit.AllowChangeTreeNode(Sender: TObject; Node: TTreeNode;
    var AllowChange: Boolean);
begin
  AllowChange := CanSelectTreeNode(Node);
end;

procedure TCustomdxVTreeViewEdit.ClickTreeView(Sender: TObject);
begin
  Click;
end;

procedure TCustomdxVTreeViewEdit.DblClickTreeView(Sender: TObject);
var
  p: TPoint;
begin
  if (VirtualTreeView.Selected <> nil)
    and CanSelectTreeNode(VirtualTreeView.Selected) then
  begin
    GetCursorPos(p);
    Windows.ScreenToClient(VirtualTreeView.Handle, p);
    if ((VirtualTreeView.GetHitTestInfoAt(p.X, p.Y) * [htOnItem, htOnIcon, htOnLabel, htOnStateIcon] <> [])
      and (VirtualTreeView.GetNodeAt(p.X, p.Y) = VirtualTreeView.Selected)) then
      CloseUp(True);
  end;
end;

procedure TCustomdxVTreeViewEdit.CloseUp(Accept: Boolean);
begin
  if FListVisible then
  begin
    FDropDownWidth := VirtualTreeView.Width;
    FDropDownRows := (VirtualTreeView.Height - 7 - TreeBorderHeight) div GetTextHeight;
    if FDropDownRows < 2 then
      FDropDownRows := 2;

    if GetCapture = VirtualTreeView.Handle then ReleaseCapture;
    FListVisible := False;
    if IsWindowVisible(VirtualTreeView.Handle) then
    begin
      SetWindowPos(VirtualTreeView.Handle, 0, 0, 0, 0, 0,
        SWP_NOZORDER or SWP_NOMOVE or SWP_NOSIZE or SWP_HIDEWINDOW);
        Windows.SetFocus(GetParentWinControl(Self).Handle);
    end;

    Invalidate;

    if Accept and not IsReadOnly then DropDownOk;
    if Assigned(FOnCloseUp) then FOnCloseUp(Self, Accept);
    if Self is TCustomdxLookupTreeView then
      TdxDBTreeView(VirtualTreeView).DataSource := nil;
  end;
end;

function TCustomdxVTreeViewEdit.IsReadOnly: Boolean;
begin
  Result := False;
end;

procedure TCustomdxVTreeViewEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    if NewStyleControls and Ctl3D then
      ExStyle := ExStyle or WS_EX_CLIENTEDGE
    else
      Style := Style or WS_BORDER;
end;

procedure TCustomdxVTreeViewEdit.DropDown;
var
  P: TPoint;
  AWidth, AHeight: Integer;
  AWorkArea: TRect;
begin
  if not FListVisible  then
  begin
    if Assigned(FOnDropDown) then FOnDropDown(Self);
    PrepareDropDown;

    P := Parent.ClientToScreen(Point(Left, Top));
    Inc(P.Y , Height);

    AHeight := FDropDownRows * GetTextHeight + 7 + TreeBorderHeight;
    if FDropDownWidth > 0 then
      AWidth := FDropDownWidth
    else
      AWidth := Width;
    AWorkArea := GetDesktopWorkArea(P);

    if P.X < AWorkArea.Left then P.X := AWorkArea.Left;
    if P.X + AWidth > AWorkArea.Right then Dec(P.X,  P.X + AWidth - AWorkArea.Right);
    if P.Y < AWorkArea.Top then P.Y := AWorkArea.Top;
    if P.Y + AHeight > AWorkArea.Bottom then Dec(P.Y, Height + AHeight);

    VirtualTreeView.SetBounds(P.X, P.Y, AWidth, AHeight);
    VirtualTreeView.UpdateSizeGripCorner;
    FListVisible := True;
    dxRecalculateNonClientPart(VirtualTreeView.Handle);
    ShowWindow(VirtualTreeView.Handle, SW_SHOWNORMAL);
    Windows.SetFocus(VirtualTreeView.Handle);
    SendMessage(GetParentWinControl(TWinControl(Owner)).Handle, WM_NCACTIVATE, WPARAM(True), 0);
    Repaint;
  end;
end;

function TCustomdxVTreeViewEdit.GetTextHeight: Integer;
begin
  Result := cxTextHeight(Font);
end;

procedure TCustomdxVTreeViewEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_DOWN) and (ssAlt in Shift) then
    DropDown;
end;

procedure TCustomdxVTreeViewEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  pt, ListPos: TPoint;
  r: TRect;
begin
  if (Button = mbLeft) and Enabled then
  begin
    SetFocus;
    if not FListVisible then
    begin
      SetRect(r, ClientWidth - FButtonWidth, 0, ClientWidth, ClientHeight);
      pt.X := X; pt.Y := Y;
      FPressed := PtInRect(r, pt);
      DropDown;
    end
    else
    begin
      pt.X := X; pt.Y := Y;
      ListPos := VirtualTreeView.ScreenToClient(ClientToScreen(pt));
      with VirtualTreeView do
        SetRect(r, 0, 0, Width, Height);
      if PtInRect(r, ListPos) then
        SendMessage(VirtualTreeView.Handle, WM_LBUTTONDOWN, 0, dxPointToLParam(ListPos))
      else
        CloseUp(False);
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomdxVTreeViewEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
  pt: TPoint;
begin
  R := ClientRect;
  R.Left := R.Right - FButtonWidth;
  pt.X := X; pt.Y := Y;
  if FPressed and not PtInRect(R, pt) then
  begin
    FPressed := False;
    InvalidateRect(Handle, @R, True);
  end;
  inherited;
end;

procedure TCustomdxVTreeViewEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if FPressed then
  begin
    FPressed := False;
    Repaint;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomdxVTreeViewEdit.Paint;
var
  W, X, Flags: Integer;
  Selected: Boolean;
  R: TRect;
  AText: string;
begin
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
  Selected := FFocused and not FListVisible and not (csPaintCopy in ControlState);
  if Selected then
  begin
    Canvas.Font.Color := clHighlightText;
    Canvas.Brush.Color := clHighlight;
  end;
  if not Enabled then
    Canvas.Font.Color := clGrayText;

  AText := GetPaintedText;
  W := ClientWidth - FButtonWidth;
  X := 2;
  case FAlignment of
    taRightJustify: X := W - Canvas.TextWidth(AText) - 3;
    taCenter: X := (W - Canvas.TextWidth(AText)) div 2;
  end;
  SetRect(R , 1, 1, W - 1, ClientHeight - 1);
  Canvas.TextRect(R, X, 2, AText);
  if Selected then
    Canvas.DrawFocusRect(R);
  SetRect(R, W , 0, ClientWidth, ClientHeight);
  Flags := DFCS_SCROLLCOMBOBOX;
  if FPressed then
    Flags := Flags or DFCS_FLAT or DFCS_PUSHED;
  if not Enabled then
    Flags := Flags or DFCS_INACTIVE;
  DrawFrameControl(Canvas.Handle, R, DFC_SCROLL, Flags);
end;

function TCustomdxVTreeViewEdit.GetPaintedText: string;
begin
  Result := FText;
end;

procedure TCustomdxVTreeViewEdit.DropDownOk;
begin
  if (VirtualTreeView <> nil) and (VirtualTreeView.Selected <> nil) then
  begin
    if (FTextStyle = tvtsFull) then
      Text := GetFullTreeNodeName(VirtualTreeView.Selected, FDividedChar)
    else
      Text := VirtualTreeView.Selected.Text;
  end;
end;

procedure TCustomdxVTreeViewEdit.PrepareDropDown;
var
  Style: Integer;
begin
  if VirtualTreeView <> nil then
    with VirtualTreeView do
    begin
      VirtualTreeView.OnGetImageIndex := FOnGetImageIndex;
      VirtualTreeView.OnGetSelectedIndex := FOnGetSelectedIndex;
      Style := GetWindowLong(Handle, GWL_STYLE);
      Style := Style or TVS_HASBUTTONS or TVS_HASLINES or TVS_LINESATROOT;
      SetWindowLong(Handle, GWL_STYLE, Style);
      if not TreeViewShowButtons then
        Style := Style and not TVS_HASBUTTONS;
      if not TreeViewShowLines then
        Style := Style and not TVS_HASLINES;
      if not TreeViewShowRoot then
        Style := Style and not TVS_LINESATROOT;
      SetWindowLong(Handle, GWL_STYLE, Style);
    end;
end;

procedure TCustomdxVTreeViewEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, GetTextHeight + GetBorderSize + 4);
end;

procedure TCustomdxVTreeViewEdit.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls then
  begin
    RecreateWnd;
    Height := 0;
  end;
  inherited;
end;

procedure TCustomdxVTreeViewEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Height := 0;
end;

procedure TCustomdxVTreeViewEdit.CMEnabledChanged(var Message: TMessage);
begin
  Invalidate;
  inherited;
end;

procedure TCustomdxVTreeViewEdit.CMMouseLeave(var Message: TMessage);
begin
  if FPressed then
  begin
    FPressed := False;
    Invalidate;
  end;
  inherited;
end;

procedure TCustomdxVTreeViewEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TCustomdxVTreeViewEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  FFocused := False;
  Invalidate;
end;

procedure TCustomdxVTreeViewEdit.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  FFocused := True;
  Invalidate;
end;

procedure TCustomdxVTreeViewEdit.SetText(Value: string);
begin
  FText := Value;
  Invalidate;
end;

procedure TCustomdxVTreeViewEdit.SetDividedChar(Value: Char);
begin
  FDividedChar := Value;
  if (VirtualTreeView <> nil) and (VirtualTreeView.Selected <> nil) then
     GetFullTreeNodeName(VirtualTreeView.Selected, FDividedChar);
  Invalidate;
end;

function TCustomdxVTreeViewEdit.CanSelectTreeNode(ANode: TTreeNode): Boolean;
begin
  Result := (ANode <> nil) and (not ANode.HasChildren or CanSelectParents);
end;

function TCustomdxVTreeViewEdit.GetCustomDraw: TTreeViewCustomDraw;
begin
  if VirtualTreeView <> nil then
    Result := VirtualTreeView.OnCustomDraw
  else
    Result := nil;
end;

function TCustomdxVTreeViewEdit.GetTreeViewColor: TColor;
begin
  if VirtualTreeView <> nil then
    Result := VirtualTreeView.Color
  else
    Result := clNone;
end;

function TCustomdxVTreeViewEdit.GetTreeViewCursor: TCursor;
begin
  if VirtualTreeView <> nil then
    Result := VirtualTreeView.Cursor
  else
    Result := crNone;
end;

function TCustomdxVTreeViewEdit.GetTreeViewFont: TFont;
begin
  if VirtualTreeView <> nil then
    Result := VirtualTreeView.Font
  else
    Result := nil;
end;

function TCustomdxVTreeViewEdit.GetTreeViewHint: string;
begin
  if VirtualTreeView <> nil then
    Result := VirtualTreeView.Hint;
end;

function TCustomdxVTreeViewEdit.GetTreeViewImages: TCustomImageList;
begin
  if VirtualTreeView <> nil then
    Result := VirtualTreeView.Images
  else Result := nil;
end;

function TCustomdxVTreeViewEdit.GetTreeViewIndent: Integer;
begin
  if VirtualTreeView <> nil then
    Result := VirtualTreeView.Indent
  else Result := -1;
end;

function TCustomdxVTreeViewEdit.GetTreeViewPopupMenu: TPopupMenu;
begin
  if VirtualTreeView <> nil then
    Result := VirtualTreeView.PopupMenu
  else Result := nil;
end;

function TCustomdxVTreeViewEdit.GetTreeViewReadOnly: Boolean;
begin
  if VirtualTreeView <> nil then
    Result := VirtualTreeView.ReadOnly
  else Result := False;
end;

function TCustomdxVTreeViewEdit.GetSelectedTreeNode: TTreeNode;
begin
  if VirtualTreeView <> nil then
    Result := VirtualTreeView.Selected
  else Result := nil;
end;

function TCustomdxVTreeViewEdit.GetTreeViewShowButtons: Boolean;
begin
  if VirtualTreeView <> nil then
    Result := VirtualTreeView.ShowButtons
  else Result := False;
end;

function TCustomdxVTreeViewEdit.GetTreeViewShowHint: Boolean;
begin
  if VirtualTreeView <> nil then
    Result := VirtualTreeView.ShowHint
  else Result := False;
end;

function TCustomdxVTreeViewEdit.GetTreeViewShowLines: Boolean;
begin
  if VirtualTreeView <> nil then
    Result := VirtualTreeView.ShowLines
  else Result := False;
end;

function TCustomdxVTreeViewEdit.GetTreeViewShowRoot: Boolean;
begin
  if VirtualTreeView <> nil then
    Result := VirtualTreeView.ShowRoot
  else Result := False;
end;

function TCustomdxVTreeViewEdit.GetTreeViewSortType: TSortType;
begin
  if VirtualTreeView <> nil then
    Result := VirtualTreeView.SortType
  else Result := stNone;
end;

function TCustomdxVTreeViewEdit.GetTreeViewStateImages: TCustomImageList;
begin
  if VirtualTreeView <> nil then
    Result := VirtualTreeView.StateImages
  else Result := nil;
end;

procedure TCustomdxVTreeViewEdit.SetCustomDraw(Value: TTreeViewCustomDraw);
begin
  if VirtualTreeView <> nil then
    VirtualTreeView.OnCustomDraw := Value;
end;

procedure TCustomdxVTreeViewEdit.SetTreeViewColor(Value: TColor);
begin
  if VirtualTreeView <> nil then
    VirtualTreeView.Color := Value;
end;

procedure TCustomdxVTreeViewEdit.SetTreeViewCursor(Value: TCursor);
begin
  if VirtualTreeView <> nil then
    VirtualTreeView.Cursor := Value;
end;

procedure TCustomdxVTreeViewEdit.SetTreeViewFont(Value: TFont);
begin
  if VirtualTreeView <> nil then
    VirtualTreeView.Font := Value;
end;

procedure TCustomdxVTreeViewEdit.SetTreeViewHint(Value: string);
begin
  if VirtualTreeView <> nil then
    VirtualTreeView.Hint := Value;
end;

procedure TCustomdxVTreeViewEdit.SetTreeViewImages(Value: TCustomImageList);
begin
  if VirtualTreeView <> nil then
    VirtualTreeView.Images := Value;
end;

procedure TCustomdxVTreeViewEdit.SetTreeViewIndent(Value: Integer);
begin
  if VirtualTreeView <> nil then
    VirtualTreeView.Indent := Value;
end;

procedure TCustomdxVTreeViewEdit.SetTreeViewPopupMenu(Value: TPopupMenu);
begin
  if VirtualTreeView <> nil then
    VirtualTreeView.PopupMenu := Value;
end;

procedure TCustomdxVTreeViewEdit.SetTreeViewReadOnly(Value: Boolean);
begin
  if VirtualTreeView <> nil then
    VirtualTreeView.ReadOnly := Value;
end;

procedure TCustomdxVTreeViewEdit.SetTreeViewShowButtons(Value: Boolean);
begin
  if VirtualTreeView <> nil then
    VirtualTreeView.ShowButtons := Value;
end;

procedure TCustomdxVTreeViewEdit.SetTreeViewShowHint(Value: Boolean);
begin
  if VirtualTreeView <> nil then
    VirtualTreeView.ShowHint := Value;
end;

procedure TCustomdxVTreeViewEdit.SetTreeViewShowLines(Value: Boolean);
begin
  if VirtualTreeView <> nil then
    VirtualTreeView.ShowLines := Value;
end;

procedure TCustomdxVTreeViewEdit.SetTreeViewShowRoot(Value: Boolean);
begin
  if VirtualTreeView <> nil then
    VirtualTreeView.ShowRoot := Value;
end;

procedure TCustomdxVTreeViewEdit.SetTreeViewSortType(Value: TSortType);
begin
  if VirtualTreeView <> nil then
    VirtualTreeView.SortType := Value;
end;

procedure TCustomdxVTreeViewEdit.SetTreeViewStateImages(Value: TCustomImageList);
begin
  if VirtualTreeView <> nil then
    VirtualTreeView.StateImages := Value;
end;

{ TPopupDBTreeView }

constructor TPopupDBTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHelper := TPopupTreeViewHelper.Create(Self);
end;

destructor TPopupDBTreeView.Destroy;
begin
  FreeAndNil(FHelper);
  inherited;
end;

procedure TPopupDBTreeView.WMActivate(var Message: TWMActivate);
begin
  inherited;
  FHelper.WMActivate(Message);
end;

procedure TPopupDBTreeView.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  FHelper.WMGetDlgCode(Message);
end;

procedure TPopupDBTreeView.WMMouseActivate(var Message: TWMMouseActivate);
begin
  inherited;
  FHelper.WMMouseActivate(Message);
end;

procedure TPopupDBTreeView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  FHelper.CreateParams(Params);
end;

procedure TPopupDBTreeView.WndProc(var Message: TMessage);
begin
  with Message do
    if (Msg  = WM_KEYDOWN) and ((wParam = VK_ESCAPE) or (wParam = VK_RETURN)) then
      TCustomdxVTreeViewEdit(Owner).CloseUp(wParam = VK_RETURN)
   else
     inherited WndProc(Message);
end;

procedure TPopupDBTreeView.CreateWnd;
begin
  inherited;
  FHelper.CreateWnd;
end;

function TPopupDBTreeView.GetOwnerControl: TWinControl;
begin
  Result := FOwnerControl;
end;

procedure TPopupDBTreeView.UpdateSizeGripCorner;
begin
  FHelper.UpdateSizeGripCorner;
end;

procedure TPopupDBTreeView.WMCaptureChanged(var Message: TMessage);
begin
  inherited;
  FHelper.WMCaptureChanged(Message);
end;

procedure TPopupDBTreeView.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
begin
  inherited;
  FHelper.WMGetMinMaxInfo(Message);
end;

procedure TPopupDBTreeView.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  FHelper.WMNCCalcSize(Message);
  inherited;
end;

procedure TPopupDBTreeView.WMNCHitTest(var Message: TWMNCHitTest);
begin
  inherited;
  FHelper.WMNCHitTest(Message);
end;

procedure TPopupDBTreeView.WMNCPaint(var Message: TWMNCPaint);
begin
  inherited;
  FHelper.WMNCPaint(Message);
end;

procedure TPopupDBTreeView.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FHelper.CMMouseLeave(Message);
end;

procedure TPopupDBTreeView.WMLButtonUp(var Message: TWMLButtonUp);
begin
  inherited;
  FHelper.WMLButtonUp(Message);
end;

procedure TPopupDBTreeView.WMNCLButtonDown(var Message: TWMNCLButtonDown);
begin
  inherited;
  FHelper.WMNCLButtonDown(Message);
end;

{ TCustomdxLookupTreeView }
constructor TCustomdxLookupTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ListLink := TDataLinkLookupTreeView.Create;
  ListLink.LookupTreeView := Self;
  FKeyFieldValue := Null;
  DBTreeView := TPopupDBTreeView.Create(Self);
  CreateVirtualTreeView;
end;

destructor TCustomdxLookupTreeView.Destroy;
begin
  ListLink.LookupTreeView := nil;
  ListLink.Free;
  inherited Destroy;
end;

function TCustomdxLookupTreeView.VirtualTreeView: TCustomdxTreeView;
begin
  Result := DBTreeView;
end;

procedure TCustomdxLookupTreeView.DropDown;
begin
  with DBTreeView do
  begin
    if GetRealParentForm(Self) <> FOldParentForm then
    begin
      FOldParentForm := GetRealParentForm(Self);
      TPopupDBTreeView(DBTreeView).RecreateWnd;
    end;
    (DBTreeView as TPopupDBTreeView).FOwnerControl := Self;
    KeyField := FKeyFieldName;
    ParentField := FParentFieldName;
    ListField := FListFieldName;
    DisplayField := FDisplayFieldName;
    OnSetDisplayItemText := FOnSetDisplayItemText;
    ImageIndexField := FImageIndexFieldName;
    StateIndexField := FStateIndexFieldName;
    DataSource := ListSource;
  end;
  if not VarIsNull(FKeyFieldValue) then
    DBTreeView.GotoKeyFieldValue(FKeyFieldValue);
  if FAssignField <> nil then
    FDropDownKeyValue := FAssignField.Value
  else
    if FKeyField <> nil then
      FDropDownKeyValue := FKeyField.Value;
  inherited DropDown;
end;

procedure TCustomdxLookupTreeView.CloseUp(Accept: Boolean);
begin
  inherited CloseUp(Accept);
  if not Accept then
    ResetDropDown;
end;

procedure TCustomdxLookupTreeView.DropDownOk;
begin
  inherited;
  FKeyFieldValue := Null;
  if FKeyField <> nil then
    FKeyFieldValue := FKeyField.Value;
  if (FTextStyle <> tvtsFull) and (FListField <> nil) then
    Text := FListField.DisplayText;
end;

function TCustomdxLookupTreeView.GetListSource: TDataSource;
begin
  Result := ListLink.DataSource;
end;

function TCustomdxLookupTreeView.GetAddNewItem: TAddNewDBTreeNodeEvent;
begin
  Result := DBTreeView.onAddNewItem;
end;

function TCustomdxLookupTreeView.GetOptions: TdxDBTreeViewOptions;
begin
  Result := DBTreeView.Options;
end;

function TCustomdxLookupTreeView.GetRootValue: Variant;
begin
  Result := DBTreeView.RootValue;
end;

procedure TCustomdxLookupTreeView.SetAddNewItem(Value: TAddNewDBTreeNodeEvent);
begin
  DBTreeView.OnAddNewItem := Value;
end;

procedure TCustomdxLookupTreeView.SetKeyField(Value: string);
begin
  if Value <> FKeyFieldName then
  begin
    FKeyFieldName := Value;
    DataLinkActiveChanged;
  end;
end;

procedure TCustomdxLookupTreeView.SetListField(Value: string);
begin
  if Value <> FListFieldName then
  begin
    FListFieldName := Value;
    DataLinkActiveChanged;
  end;
end;

procedure TCustomdxLookupTreeView.SetOptions(Value: TdxDBTreeViewOptions);
begin
  DBTreeView.Options := Value;
end;

procedure TCustomdxLookupTreeView.SetRootValue(Value: Variant);
begin
  DBTreeView.RootValue := Value;
end;

procedure TCustomdxLookupTreeView.SetParentField(Value: string);
begin
  if Value <> FParentFieldName then
  begin
    FParentFieldName := Value;
    DataLinkActiveChanged;
  end;
end;

procedure TCustomdxLookupTreeView.SetListSource(Value: TDataSource);
begin
  if Value <> ListLink.DataSource then
    ListLink.DataSource := Value;
end;

procedure TCustomdxLookupTreeView.SetAssignField(Value: string);
begin
  if Value <> FAssignFieldName then
  begin
    FAssignFieldName := Value;
    DataLinkActiveChanged;
  end;
end;

procedure TCustomdxLookupTreeView.DataLinkActiveChanged;
begin
  FKeyField := nil;
  FListField := nil;
  FAssignField := nil;
  FParentField := nil;
  if ListLink.Active then
  begin
    if FKeyFieldName <> '' then
      FKeyField := ListLink.DataSet.FieldByName(FKeyFieldName);
    if FListFieldName <> '' then
      FListField := ListLink.DataSet.FieldByName(FListFieldName);
    if FAssignFieldName <> '' then
      FAssignField := ListLink.DataSet.FieldByName(FAssignFieldName);
    if FParentFieldName <> '' then
      FParentField := ListLink.DataSet.FieldByName(FParentFieldName);
    if FKeyField <> nil then
      FKeyFieldValue := FKeyField.Value;
    if FListField <> nil then
      Text := FListField.DisplayText;
  end;
end;

procedure TCustomdxLookupTreeView.ResetDropDown;
begin
  if FAssignField <> nil then
    DBTrDataSetLocate(ListLink.DataSet, FAssignFieldName, FDropDownKeyValue, [])
    else
      if FKeyField <> nil then
       DBTrDataSetLocate(ListLink.DataSet, FKeyFieldName, FDropDownKeyValue, []);
end;

{TPopupTreeView}
constructor TPopupTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHelper := TPopupTreeViewHelper.Create(Self);
end;

destructor TPopupTreeView.Destroy;
begin
  FreeAndNil(FHelper);
  inherited;
end;

procedure TPopupTreeView.WMActivate(var Message: TWMActivate);
begin
  inherited;
  FHelper.WMActivate(Message);
end;

procedure TPopupTreeView.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  FHelper.WMGetDlgCode(Message);
end;

procedure TPopupTreeView.WMMouseActivate(var Message: TWMMouseActivate);
begin
  inherited;
  FHelper.WMMouseActivate(Message);
end;

procedure TPopupTreeView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  FHelper.CreateParams(Params);
end;

procedure TPopupTreeView.WndProc(var Message: TMessage);
begin
  with Message do
    if (Msg  = WM_KEYDOWN) and ((wParam = VK_ESCAPE) or (wParam = VK_RETURN)) then
      TCustomdxVTreeViewEdit(Owner).CloseUp(wParam = VK_RETURN)
    else
    begin
      if (Msg = WM_DESTROY) and (Owner <> nil) and not cxIsDestroying(Self, True) then
        TCustomdxTreeViewEdit(Owner).SaveNodesToStream;
      inherited WndProc(Message);
    end;
end;

procedure TPopupTreeView.CreateWnd;
begin
  inherited;
  FHelper.CreateWnd;
end;

function TPopupTreeView.GetOwnerControl: TWinControl;
begin
  Result := FOwnerControl;
end;

procedure TPopupTreeView.UpdateSizeGripCorner;
begin
  FHelper.UpdateSizeGripCorner;
end;

procedure TPopupTreeView.WMCaptureChanged(var Message: TMessage);
begin
  inherited;
  FHelper.WMCaptureChanged(Message);
end;

procedure TPopupTreeView.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
begin
  inherited;
  FHelper.WMGetMinMaxInfo(Message);
end;

procedure TPopupTreeView.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  if FHelper <> nil then
    FHelper.WMNCCalcSize(Message);
  inherited;
end;

procedure TPopupTreeView.WMNCHitTest(var Message: TWMNCHitTest);
begin
  inherited;
  FHelper.WMNCHitTest(Message);
end;

procedure TPopupTreeView.WMNCPaint(var Message: TWMNCPaint);
begin
  inherited;
  FHelper.WMNCPaint(Message);
end;

procedure TPopupTreeView.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FHelper.CMMouseLeave(Message);
end;

procedure TPopupTreeView.WMLButtonUp(var Message: TWMLButtonUp);
begin
  inherited;
  FHelper.WMLButtonUp(Message);
end;

procedure TPopupTreeView.WMNCLButtonDown(var Message: TWMNCLButtonDown);
begin
  inherited;
  FHelper.WMNCLButtonDown(Message);
end;

constructor TPopupTreeViewHelper.Create(ATreeView: TCustomdxTreeView);
begin
  inherited Create;
  FTreeView := ATreeView;
  FTreeViewEdit := FTreeView.Owner as TCustomdxVTreeViewEdit;

  FTreeView.ControlStyle := FTreeView.ControlStyle + [csNoDesignVisible, csReplicatable];
  TCustomdxTreeViewAccess(FTreeView).DragMode := dmManual;
end;

procedure TPopupTreeViewHelper.WMActivate(var Message: TWMActivate);
begin
  if (Message.Active = WA_INACTIVE) and FTreeViewEdit.FListVisible then
    FTreeViewEdit.CloseUp(False);
end;

procedure TPopupTreeViewHelper.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := Message.Result + DLGC_WANTALLKEYS;
end;

procedure TPopupTreeViewHelper.WMMouseActivate(var Message: TWMMouseActivate);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TPopupTreeViewHelper.WMNCCalcSize(var Message: TWMNCCalcSize);

  function GetSysPanelBorders: TRect;
  begin
    Result := cxNullRect;
    if FCorner in [coBottomLeft, coBottomRight] then
      Result.Top := 1
    else
      Result.Bottom := 1;
  end;

var
  AClientRect, APanelRect: TRect;
begin
  if FTreeViewEdit.FListVisible then
  begin
    AClientRect := Message.CalcSize_Params.rgrc[0];
    cxLookAndFeelPaintersManager.GetPainter(lfsFlat).CalculateScaledPopupPanelClientRect(
      AClientRect, APanelRect, FGripRect, FCloseButtonRect, FCorner, cxSimpleRect, GetSysPanelBorders, dxSystemScaleFactor);
    Message.CalcSize_Params.rgrc[0] := cxRectInflate(AClientRect, 1, 1);
  end;
end;

procedure TPopupTreeViewHelper.WMNCHitTest(var Message: TWMNCHitTest);
var
  PrevMouseAboveCloseButton: Boolean;
begin
  with Message do
    if PtInRect(FGripRect, SmallPointToPoint(Pos)) then
      Result := GetHitTestByCorner(FCorner)
    else
    begin
      PrevMouseAboveCloseButton := FMouseAboveCloseButton;
      FMouseAboveCloseButton := PtInRect(FCloseButtonRect, SmallPointToPoint(Pos));
      if FMouseAboveCloseButton then Result := HTBORDER;
      if PrevMouseAboveCloseButton <> FMouseAboveCloseButton then
        SendMessage(FTreeView.Handle, WM_NCPAINT, 0, 0);
    end;
end;

procedure TPopupTreeViewHelper.WMNCPaint(var Message: TWMNCPaint);
begin
  if FTreeViewEdit.FListVisible then
    cxLookAndFeelPaintersManager.GetPainter(lfsFlat).DrawScaledPopupNCPanel(
      FTreeView.Handle, FMouseAboveCloseButton, FCloseButtonIsTracking, FCorner,
      FCloseButtonRect, FGripRect, clWindowFrame, dxSystemScaleFactor);
end;

procedure TPopupTreeViewHelper.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
begin
  Message.MinMaxInfo^.ptMinTrackSize := Point(100, 100);
end;

procedure TPopupTreeViewHelper.WMCaptureChanged(var Message: TMessage);
begin
  if FCloseButtonIsTracking then
  begin
    FCloseButtonIsTracking := False;
    FMouseAboveCloseButton := False;
    SendMessage(FTreeView.Handle, WM_NCPAINT, 0, 0);
  end;
end;

procedure TPopupTreeViewHelper.WMLButtonUp(var Message: TWMLButtonUp);
begin
  if FCloseButtonIsTracking then
  begin
    FCloseButtonIsTracking := False;
    ReleaseCapture;
    if FMouseAboveCloseButton then
      FTreeViewEdit.CloseUp(False)
    else
      SendMessage(FTreeView.Handle, WM_NCPAINT, 0, 0);
  end;
end;

procedure TPopupTreeViewHelper.WMNCLButtonDown(var Message: TWMNCLButtonDown);
begin
  if FMouseAboveCloseButton then
  begin
    FCloseButtonIsTracking := True;
    SetCapture(FTreeView.Handle);
    SendMessage(FTreeView.Handle, WM_NCPAINT, 0, 0);
  end;
end;

procedure TPopupTreeViewHelper.CMMouseLeave(var Message: TMessage);
begin
  if FMouseAboveCloseButton then
  begin
    FMouseAboveCloseButton := False;
    SendMessage(FTreeView.Handle, WM_NCPAINT, 0, 0);
  end;
end;

procedure TPopupTreeViewHelper.CreateParams(var Params: TCreateParams);
begin
  with Params do
  begin
    Style := Style and not WS_CHILD or WS_POPUP or WS_BORDER;
    ExStyle := ExStyle and not WS_EX_CLIENTEDGE;

    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
    WndParent := GetParentWinControl(FTreeViewEdit).Handle;
  end;
end;

procedure TPopupTreeViewHelper.CreateWnd;
begin
  FHScrollWidth := GetSystemMetrics(SM_CYHSCROLL);
  FVScrollWidth := GetSystemMetrics(SM_CXVSCROLL);
end;

procedure TPopupTreeViewHelper.UpdateSizeGripCorner;
var
  AEditRect, ADropDownRect: TRect;
begin
  AEditRect := cxGetWindowRect(FTreeViewEdit.Handle);
  ADropDownRect := cxGetWindowRect(FTreeView.Handle);
  FCorner := GetCornerForRects(AEditRect, ADropDownRect);
end;

{ TdxDBLookupTreeView }

constructor TdxDBLookupTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnActiveChange := ActiveChange;
  FDataLink.OnUpdateData := UpdateData;
  FCloseUpFlag := False;
end;

destructor TdxDBLookupTreeView.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  FCanvas.Free;
  inherited Destroy;
end;

procedure TdxDBLookupTreeView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TdxDBLookupTreeView.SearchTreeNode;
begin
  if (FDataLink.Field <> nil) and (DBTreeView <> nil) then
    DBTreeView.GotoKeyFieldValue(FDataLink.Field.Value);
end;

procedure TdxDBLookupTreeView.PrepareDropDown;
begin
  inherited;
  SearchTreeNode;
end;

procedure TdxDBLookupTreeView.ActiveChange(Sender: TObject);
begin
  CheckNotCircular;
end;

procedure TdxDBLookupTreeView.DataChange(Sender: TObject);
begin
  if FUpdateDataFlag then Exit;

  if (FListField <> nil) and (FAlignment <> FListField.Alignment) then
    FAlignment := FListField.Alignment;
  if FDataLink.Field <> nil then
    Text := GetDisplayText
  else
    if csDesigning in ComponentState then
      Text := Name
    else
      Text := '';
end;

procedure TdxDBLookupTreeView.UpdateData(Sender: TObject);
begin
  FUpdateDataFlag := True;
  if FDataLink.Active then
  begin
    if not FDataLink.Editing then
      FDataLink.Edit;
    if (FDataLink.DataSet.State = dsInsert) or (FDataLink.dataSet.State = dsEdit) then
      FDataLink.Field.Value := GetLookupValue;
  end;
  FUpdateDataFlag := False;
end;

procedure TdxDBLookupTreeView.DropDown;
begin
  inherited DropDown;
  FDataLink.Modified;
end;

procedure TdxDBLookupTreeView.DataLinkActiveChanged;
begin
  CheckNotCircular;
  inherited DataLinkActiveChanged;
  DataChange(nil);
end;

procedure TdxDBLookupTreeView.DropDownOk;
begin
  inherited DropDownOk;
  UpdateData(Self);
end;

function TdxDBLookupTreeView.GetPaintedText: string;
begin
  Result := GetDisplayText;
end;

function TdxDBLookupTreeView.IsReadOnly: Boolean;
begin
  Result := ReadOnly;
end;

procedure TdxDBLookupTreeView.CloseUp(Accept: Boolean);
begin
  if FCloseUpFlag then Exit;
  FCloseUpFlag := True;
  inherited CloseUp(Accept);
  FCloseUpFlag := False;
end;

function TdxDBLookupTreeView.GetDisplayText: string;
var
  b: Boolean;
  AKeyValue: Variant;
begin
  Result := '';
  if (FDataLink.Field = nil) or (FKeyField = nil) or (FListField = nil) or (FParentField = nil) then
    Exit;
  if ListLink.Active then
  begin
    ListLink.DataSet.DisableControls;
    if FAssignField <> nil then
      b:= DBTrDataSetLocate(ListLink.DataSet, FAssignFieldName, FDataLink.Field.Value, [])
    else
      b := DBTrDataSetLocate(ListLink.DataSet, FKeyFieldName, FDataLink.Field.Value, []);
    if b then
    begin
      Result := FListField.Text;
      if FTextStyle = tvtsFull then
      begin
        AKeyValue := FKeyField.Value;
        while DBTrDataSetLocate(ListLink.DataSet, FKeyFieldName, FParentField.Value, [])
          and not VarEquals(FParentField.Value, FKeyField.Value) do
          Result := FListField.Text + FDividedChar + Result;
        DBTrDataSetLocate(ListLink.DataSet, FKeyFieldName, AKeyValue, []);
      end;
    end;
    ListLink.DataSet.EnableControls;
  end;
end;

function TdxDBLookupTreeView.GetLookupValue: Variant;
begin
  if not ListLink.Active then
    Result := Null
  else
    if FAssignField <> nil then
      Result := FAssignField.Value
    else
      Result := FKeyField.Value;
end;

function TdxDBLookupTreeView.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TdxDBLookupTreeView.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

function TdxDBLookupTreeView.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TdxDBLookupTreeView.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TdxDBLookupTreeView.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TdxDBLookupTreeView.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TdxDBLookupTreeView.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TdxDBLookupTreeView.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TdxDBLookupTreeView.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(FDataLink);
end;

procedure TdxDBLookupTreeView.CheckNotCircular;
begin
  if FDataLink.Active and ListLink.Active and (FDataLink.DataSet.IsLinkedTo(ListSource)
    or ListLink.DataSet.IsLinkedTo(DataSource)) then
    DatabaseError(SCircularDataLink);
end;

end.


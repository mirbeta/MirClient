{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressLayoutControl main components                     }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSLAYOUTCONTROL AND ALL          }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxLayoutImport;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, cxClasses, dxLayoutControl, cxControls, cxStyles,
  cxGraphics, ExtCtrls, cxContainer, cxLabel, cxLookAndFeelPainters, cxGroupBox,
  Types, cxLookAndFeels, cxSplitter, Menus, cxDropDownEdit, cxCheckBox, cxButtons,
  cxRadioGroup, cxEdit, cxTextEdit, cxMaskEdit, cxSpinEdit, ComCtrls, cxPC,
  dxLayoutcxEditAdapters, dxLayoutLookAndFeels, dxLayoutContainer,
  dxLayoutControlAdapters;

type
  TdxAutoLayout = class;

  { TdxAutoLayoutItem }

  TdxAutoLayoutItem = class
  private
    FAlign: TAlign;
    FAnchors: TAnchors;
    FAutoLayout: TdxAutoLayout;
    FBoundsRect: TRect;
    FCaption: string;
    FCaptionLayout: TdxCaptionLayout;
    FControl: TControl;
    FLabeled: Boolean;
    FLabeledBoundsRect: TRect;
    FLayoutControlItem: TdxCustomLayoutItem;
    FLayoutHorizontalCenter: Integer;
    procedure SetAligns;
  public
    constructor Create(AAutoLayout: TdxAutoLayout; AControl: TControl);
    procedure Apply(ANeedSetAligns: Boolean = True);
    procedure Assign(Source: TdxAutoLayoutItem);
    procedure CreateLayoutControlItem(AGroup: TdxLayoutGroup);
    function CanResize: Boolean;

    function CreateGroup(AGroup: TdxLayoutGroup): TdxLayoutGroup;
    function CreateItem(AGroup: TdxLayoutGroup): TdxCustomLayoutItem;
    function CreateSeparator(AGroup: TdxLayoutGroup): TdxCustomLayoutItem;
    function CreateSplitter(AGroup: TdxLayoutGroup): TdxCustomLayoutItem;
    function CreateTabbedGroup(AGroup: TdxLayoutGroup): TdxCustomLayoutItem;
    procedure PopulateTabbedGroup(AControlGroup: TdxLayoutGroup);

    function GetGroupCaption: string;
    function GetLabelCaption: string;
    function IsAligned: Boolean;
    function IsButton: Boolean;
    function IsGroup: Boolean;
    function IsHiddenGroup: Boolean;
    function IsLabel: Boolean;
    function IsSeparator: Boolean;
    function IsSplitter: Boolean;
    function IsPageControl: Boolean;
    function IsStandaloneLabel: Boolean;
    function GetFocusControl: TWinControl;
    procedure SetLabelInfo(ALabel: TControl);

    property BoundsRect: TRect read FBoundsRect;
    property Caption: string read FCaption;
    property CaptionLayout: TdxCaptionLayout read FCaptionLayout;
    property Control: TControl read FControl;
    property Labeled: Boolean read FLabeled;
    property LabeledBoundsRect: TRect read FLabeledBoundsRect;
    property LayoutControlItem: TdxCustomLayoutItem read FLayoutControlItem;
    property LayoutHorizontalCenter: Integer read FLayoutHorizontalCenter write FLayoutHorizontalCenter;
  end;

  { TdxAutoLayoutItemList }

  TdxAutoLayoutItemList = class(TList)
  private
    function GetItem(Index: Integer): TdxAutoLayoutItem;
  public
    function CanConvert(ASide: TcxBorder): Boolean;
    function First: TdxAutoLayoutItem;
    procedure FreeItems;
    function IsButtons: Boolean;
    function Last: TdxAutoLayoutItem;

    property Items[Index: Integer]: TdxAutoLayoutItem read GetItem; default;
  end;

  { TdxAutoLayoutOwnedItemList }

  TdxAutoLayoutOwnedItemList = class(TcxObjectList)
  private
    FAutoLayout: TdxAutoLayout;
    function GetItem(Index: Integer): TdxAutoLayoutItem;
  public
    constructor Create(AAutoLayout: TdxAutoLayout);
    function Add(AControl: TControl): TdxAutoLayoutItem;
    function AddAlignedItems(AItem: TdxAutoLayoutItem): TdxAutoLayoutItem;
    procedure DeleteItems(AItems: TdxAutoLayoutItemList);
    procedure Extract(AIndex: Integer);
    function Find(AControl: TControl): TdxAutoLayoutItem;
    function Remove(AControl: TControl): Boolean;
    procedure RemoveItems(AItems: TdxAutoLayoutItemList);

    property Items[Index: Integer]: TdxAutoLayoutItem read GetItem; default;
  end;

  { TdxAutoLayoutHelper }

  TdxAutoLayoutHelper = class
  private
    FAlignedItems: TdxAutoLayoutOwnedItemList;
    FAligns: set of TAlign;
    FBottommostItem: TdxAutoLayoutItem;
    FContainer: TWinControl;
    FItems: TdxAutoLayoutOwnedItemList;
    FLeftmostItem: TdxAutoLayoutItem;
    FNonAlignedBoundsRect: TRect;
    FOwner: TdxAutoLayout;
    FRightmostItem: TdxAutoLayoutItem;
    FRoot: TdxLayoutGroup;
    FTopmostItem: TdxAutoLayoutItem;
    function CanExport(AControl: TControl): Boolean;
    function FindBottommostItem: TdxAutoLayoutItem;
    function FindLeftmostItem: TdxAutoLayoutItem;
    function FindRightmostItem: TdxAutoLayoutItem;
    function FindTopmostItem: TdxAutoLayoutItem;
    function GetControlAtPos(X, Y: Integer): TControl;
    function GetFocusControl(AItem: TdxAutoLayoutItem): TWinControl;
    function GetItemCount: Integer;
    function HasItemsAtRect(const R: TRect): Boolean;
    procedure MakeNewRoot;
    function NewGroup(AItems: TdxAutoLayoutItemList): TdxLayoutGroup;
    procedure ProcessAnchors(AItems: TdxAutoLayoutItemList;
      out AAlignHorz: TdxLayoutAlignHorz; out AAlignVert: TdxLayoutAlignVert);
    procedure PopulateItems;
    procedure PopulateItemsAtRect(AItems: TdxAutoLayoutItemList; const R: TRect);
    procedure ProcessLabels;
    function UpdateUtmostItems: TRect;
  protected
    procedure CheckRootDirection(AItems: TdxAutoLayoutItemList; ADirection: TdxLayoutDirection);
    procedure ConvertGroup(AItems: TdxAutoLayoutItemList; ASide: TcxBorder;
      const APreviousGroupBounds: TRect; out AGroupBounds: TRect);
    procedure DoNonAlignedControls;
    procedure DoExecute; virtual;
    procedure GenerateAligned(AAlign: TAlign);
    procedure GenerateLayout; virtual;
    procedure GenerateNonAlignedLayout; virtual;
    procedure GetSideItemGroup(AItems: TdxAutoLayoutItemList; ASide: TcxBorder);
    procedure Initialize(AContainer: TWinControl; ARoot: TdxLayoutGroup); virtual;

    property Owner: TdxAutoLayout read FOwner;
    property Root: TdxLayoutGroup read FRoot;
  public
    constructor Create(AOwner: TdxAutoLayout);
    destructor Destroy; override;
    procedure Execute(AContainer: TWinControl; ARoot: TdxLayoutGroup);

    property Container: TWinControl read FContainer;
    property ItemCount: Integer read GetItemCount;
    property Items: TdxAutoLayoutOwnedItemList read FItems;
  end;

  { TdxAutoLayout }

  TdxAutoLayout = class
  private
    FFocusControlDistanceX: Byte;
    FFocusControlDistanceY: Byte;
    FControl: TdxCustomLayoutControl;
    FConvertPageControls: Boolean;
    FRecursive: Boolean;
    FSmartFindFocusControl: Boolean;
    FUseLabeledItems: Boolean;
  protected
    procedure CorrectAlign(AGroup: TdxLayoutGroup);
    procedure DoExecute(AContainer: TWinControl; ARecursive: Boolean = True); virtual;

    property Control: TdxCustomLayoutControl read FControl;
    property Recursive: Boolean read FRecursive;
  public
    constructor Create(AControl: TdxCustomLayoutControl);
    procedure Execute(AContainer: TWinControl; ARecursive: Boolean = True);

    property ConvertPageControls: Boolean read FConvertPageControls write FConvertPageControls default True;
    property FocusControlDistanceX: Byte read FFocusControlDistanceX write FFocusControlDistanceX default 48;
    property FocusControlDistanceY: Byte read FFocusControlDistanceY write FFocusControlDistanceY default 16;
    property SmartFindFocusControl: Boolean read FSmartFindFocusControl write FSmartFindFocusControl default True;
    property UseLabeledItems: Boolean read FUseLabeledItems write FUseLabeledItems default True;
  end;

  { TfmImport }

  TfmImport = class(TForm)
    seDeltaX: TcxSpinEdit;
    seDeltaY: TcxSpinEdit;
    btnImport: TcxButton;
    btnCancel: TcxButton;
    cbContainers: TcxComboBox;
    cbUseLabeledItems: TcxCheckBox;
    cbAssociate: TcxCheckBox;
    cbConvertPageControls: TcxCheckBox;
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutControl1: TdxLayoutControl;
    dxLayoutControl1Item1: TdxLayoutItem;
    dxLayoutControl1Item2: TdxLayoutItem;
    dxLayoutControl1Item3: TdxLayoutItem;
    dxLayoutControl1Group2: TdxLayoutGroup;
    dxLayoutControl1Item4: TdxLayoutItem;
    dxLayoutControl1Item5: TdxLayoutItem;
    dxLayoutControl1Group4: TdxLayoutGroup;
    dxLayoutControl1Item6: TdxLayoutItem;
    dxLayoutControl1Item7: TdxLayoutItem;
    dxLayoutControl1Item8: TdxLayoutItem;
    dxLayoutControl1LabeledItem1: TdxLayoutLabeledItem;
    dxLayoutControl1LabeledItem2: TdxLayoutLabeledItem;
    dxLayoutControl1Group5: TdxLayoutGroup;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel2: TdxLayoutCxLookAndFeel;
    procedure cbAssociateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    function GetImportContainer: TWinControl;
    procedure PopulateContainersList(ALayoutControl: TdxCustomLayoutControl; AParentContainer: TWinControl);
    function CanExport(ALayoutControl: TdxCustomLayoutControl; AContainer: TWinControl): Boolean;
  end;

procedure ImportLayout(ALayoutControl: TdxLayoutControl; ADefaultImport: Boolean = False); overload;

implementation

uses
  cxGeometry, Math;

{$R *.dfm}

const
  BorderAlignmentAccuracy = 10;
  CenterAlignmentAccuracy = 24;

type
  TControlAccess = class(TControl);
  TWinControlAccess = class(TWinControl);
  TLabelAccess = class(TCustomLabel);
  TcxLabelAccess = class(TcxCustomLabel);
  TdxLayoutGroupAcsess = class(TdxLayoutGroup);
  TdxLayoutControlAccess = class(TdxLayoutControl);
  TdxCustomLayoutItemAccess = class(TdxCustomLayoutItem);
  TdxLayoutItemAccess = class(TdxLayoutItem);
  TStaticTextAccess = class(TCustomStaticText);
  TcxGroupBoxAccess = class(TcxCustomGroupBox);
  TGroupBoxAccess = class(TCustomGroupBox);
  TPanelAccess = class(TCustomPanel);

  { TdxSideItem }

  TdxSideItem = class
  private
    FItems: TdxAutoLayoutItemList;
    FRoot: TdxLayoutGroup;
    FSide: TcxBorder;
    function GetDirection: TdxLayoutDirection;
  protected
    FOrder: Integer;
  public
    constructor Create(AItems: TdxAutoLayoutItemList; ASide: TcxBorder);
    destructor Destroy; override;

    property Direction: TdxLayoutDirection read GetDirection;
    property Items: TdxAutoLayoutItemList read FItems write FItems;
    property Side: TcxBorder read FSide write FSide;
  end;

  { TdxSideItem }

  TdxSideItemList = class(TcxObjectList)
  private
    FHelper: TdxAutoLayoutHelper;
    function GetItem(Index: Integer): TdxSideItem;
  protected
    property Helper: TdxAutoLayoutHelper read FHelper;
  public
    constructor Create(AHelper: TdxAutoLayoutHelper);
    function Add(AItems: TdxAutoLayoutItemList; ASide: TcxBorder): TdxSideItem;
    procedure BuildLayout(out APreviousGroupBounds: TRect);

    property Items[Index: Integer]: TdxSideItem read GetItem; default;
  end;

const
  AlignHorzs: array[Boolean, Boolean] of TdxLayoutAlignHorz =
    ((ahLeft, ahRight), (ahLeft, ahClient));
  AlignVerts: array[Boolean, Boolean] of TdxLayoutAlignVert =
    ((avTop, avBottom), (avTop, avClient));

function CompareTop(Item1, Item2: Pointer): Integer;
begin
  Result := TdxAutoLayoutItem(Item1).BoundsRect.Top - TdxAutoLayoutItem(Item2).BoundsRect.Top;
end;

function CompareBottom(Item1, Item2: Pointer): Integer;
begin
  Result := TdxAutoLayoutItem(Item1).BoundsRect.Bottom - TdxAutoLayoutItem(Item2).BoundsRect.Bottom;
end;

function CompareLeft(Item1, Item2: Pointer): Integer;
begin
  Result := TdxAutoLayoutItem(Item1).BoundsRect.Left - TdxAutoLayoutItem(Item2).BoundsRect.Left;
end;

function CompareRight(Item1, Item2: Pointer): Integer;
begin
  Result := TdxAutoLayoutItem(Item1).BoundsRect.Right - TdxAutoLayoutItem(Item2).BoundsRect.Right;
end;

{ TdxSideItem }

constructor TdxSideItem.Create(AItems: TdxAutoLayoutItemList; ASide: TcxBorder);
var
  I: Integer;
begin
  FSide := ASide;
  FItems := TdxAutoLayoutItemList.Create;
  for I := 0 to AItems.Count - 1 do
    FItems.Add(AItems[I]);
end;

destructor TdxSideItem.Destroy;
begin
  FItems.FreeItems;
  FItems.Free;
  inherited Destroy;
end;

function TdxSideItem.GetDirection: TdxLayoutDirection;
begin
  if FSide in [bTop, bBottom] then
    Result := ldHorizontal
  else
    Result := ldVertical;
end;

{ TdxSideItemList }

constructor TdxSideItemList.Create(AHelper: TdxAutoLayoutHelper);
begin
  inherited Create;
  FHelper := AHelper;
end;

function TdxSideItemList.Add(AItems: TdxAutoLayoutItemList;
  ASide: TcxBorder): TdxSideItem;
begin
  Result := TdxSideItem.Create(AItems, ASide);
  inherited Add(Result);
  Helper.Items.DeleteItems(AItems);
end;

procedure TdxSideItemList.BuildLayout(out APreviousGroupBounds: TRect);

  procedure GenerateRoot(AIndex: Integer);
  var
    I, J: Integer;
    L: TdxAutoLayoutItemList;
  begin
    if Items[AIndex].Direction <> Helper.Root.LayoutDirection then
      Exit;
    L := TdxAutoLayoutItemList.Create;
    try
      for I := AIndex to Count - 1 do
        for J := 0 to Items[I].Items.Count - 1 do
          L.Add(Items[I].Items[J]);
      Helper.CheckRootDirection(L, Items[AIndex].Direction);
    finally
      L.Free;
    end;
  end;

  procedure BuildLeftTopSide;
  var
    I: Integer;
    AGroupBounds: TRect;
  begin
    for I := 0 to Count - 1 do
    begin
      GenerateRoot(I);
      if Items[I].Side in [bLeft, bTop] then
      begin
        Helper.ConvertGroup(Items[I].Items, Items[I].Side, APreviousGroupBounds, AGroupBounds);
        APreviousGroupBounds := AGroupBounds;
      end
      else
        Items[I].FRoot := Helper.FRoot;
    end;
    for I := Count - 1 downto 0 do
      if Items[I].Side in [bLeft, bTop] then
      begin
        Items[I].Free;
        Delete(I);
      end;
  end;

  procedure BuildRightBottomSide;
  var
    I: Integer;
    AGroupBounds: TRect;
  begin
    for I := Count - 1 downto 0 do
    begin
      Helper.FRoot := Items[I].FRoot;
      Helper.ConvertGroup(Items[I].Items, Items[I].Side, APreviousGroupBounds, AGroupBounds);
      APreviousGroupBounds := AGroupBounds;
    end;
  end;

begin
  APreviousGroupBounds := cxNullRect;
  BuildLeftTopSide;
  BuildRightBottomSide;
end;

function TdxSideItemList.GetItem(Index: Integer): TdxSideItem;
begin
  Result := TdxSideItem(inherited Items[Index]);
end;

{ TdxAutoLayoutControl }

constructor TdxAutoLayoutItem.Create(AAutoLayout: TdxAutoLayout; AControl: TControl);
begin
  FAutoLayout := AAutoLayout;
  FControl := AControl;
  FBoundsRect := Control.BoundsRect;
  FLabeledBoundsRect := FBoundsRect;
  FAlign := Control.Align;
  FAnchors := Control.Anchors;
  FLayoutHorizontalCenter := MaxInt;
end;

procedure TdxAutoLayoutItem.Assign(Source: TdxAutoLayoutItem);
begin
  FControl := Source.Control;
  FBoundsRect := Source.BoundsRect;
  FLabeledBoundsRect := Source.FLabeledBoundsRect;
  FAlign := Source.FAlign;
end;

procedure TdxAutoLayoutItem.CreateLayoutControlItem(AGroup: TdxLayoutGroup);
begin
  if IsPageControl then
    FLayoutControlItem := CreateTabbedGroup(AGroup)
  else if IsGroup then
    FLayoutControlItem := CreateGroup(AGroup)
  else if IsSplitter then
    FLayoutControlItem := CreateSplitter(AGroup)
  else if IsSeparator then
    FLayoutControlItem := CreateSeparator(AGroup)
  else
    FLayoutControlItem := CreateItem(AGroup);
end;

function TdxAutoLayoutItem.CanResize: Boolean;
begin
  Result := not IsButton;
end;

function TdxAutoLayoutItem.CreateGroup(AGroup: TdxLayoutGroup): TdxLayoutGroup;
begin
  Result := AGroup.CreateGroup;
  if AGroup.LayoutDirection = ldVertical then
    Result.AlignHorz := ahClient
  else
    Result.AlignVert := avClient;
  Result.Caption := GetGroupCaption;
  Result.Hidden := IsHiddenGroup;
  if FAutoLayout.Recursive and (TWinControl(Control).ControlCount > 0) then
  begin
    with TdxAutoLayoutHelper.Create(FAutoLayout) do
    try
      Execute(TWinControl(Control), Result);
    finally
      Free;
    end;
  end;
end;

function TdxAutoLayoutItem.CreateItem(AGroup: TdxLayoutGroup): TdxCustomLayoutItem;
begin
  if FAutoLayout.UseLabeledItems and IsStandaloneLabel then
  begin
    Result := AGroup.CreateItem(TdxLayoutLabeledItem);
    Result.Caption := GetLabelCaption;
  end
  else
    Result := AGroup.CreateItemForControl(Control);
end;

function TdxAutoLayoutItem.CreateSeparator(AGroup: TdxLayoutGroup): TdxCustomLayoutItem;
begin
  Result := AGroup.CreateItem(TdxLayoutSeparatorItem);
end;

function TdxAutoLayoutItem.CreateSplitter(AGroup: TdxLayoutGroup): TdxCustomLayoutItem;
begin
  Result := AGroup.CreateItem(TdxLayoutSplitterItem);
end;

function TdxAutoLayoutItem.CreateTabbedGroup(AGroup: TdxLayoutGroup): TdxCustomLayoutItem;
var
  AControlGroup: TdxLayoutGroup;
begin
  AControlGroup := AGroup.CreateGroup;
  if AGroup.LayoutDirection = ldVertical then
    AControlGroup.AlignHorz := ahClient
  else
    AControlGroup.AlignVert := avClient;
  if not FAutoLayout.ConvertPageControls then
  begin
    Result := CreateItem(AControlGroup);
    Exit;
  end;
  PopulateTabbedGroup(AControlGroup);
  Result := AControlGroup;
end;

procedure TdxAutoLayoutItem.PopulateTabbedGroup(AControlGroup: TdxLayoutGroup);
var
  AGroup: TdxLayoutGroup;
  AcxPageControl: TcxPageControl;
  APageControl: TPageControl;
  I, APageIndex: Integer;
begin
  AControlGroup.LayoutDirection := ldTabbed;
  if FAutoLayout.Recursive then
  begin
    if Control is TPageControl then
    begin
      APageControl := TPageControl(Control);
      APageIndex := APageControl.ActivePageIndex;
      for I := 0 to APageControl.PageCount - 1 do
      begin
        APageControl.ActivePageIndex := I;
        Application.ProcessMessages;
        AGroup := AControlGroup.CreateGroup;
        AGroup.Caption := APageControl.Pages[I].Caption;
        with TdxAutoLayoutHelper.Create(FAutoLayout) do
        try
          Execute(APageControl.Pages[I], AGroup);
        finally
          Free;
        end;
      end;
      AControlGroup.ItemIndex := APageIndex;
    end
    else
      if Control is TcxPageControl then
      begin
        AcxPageControl := TcxPageControl(Control);
        APageIndex := AcxPageControl.ActivePageIndex;
        for I := 0 to AcxPageControl.PageCount - 1 do
        begin
          AcxPageControl.ActivePageIndex := I;
          Application.ProcessMessages;
          AGroup := AControlGroup.CreateGroup;
          AGroup.Caption := AcxPageControl.Pages[I].Caption;
          with TdxAutoLayoutHelper.Create(FAutoLayout) do
          try
            Execute(AcxPageControl.Pages[I], AGroup);
          finally
            Free;
          end;
        end;
        AControlGroup.ItemIndex := APageIndex;
      end
  end;
end;

function TdxAutoLayoutItem.GetGroupCaption: string;
begin
  if Control is TCustomGroupBox then
    Result := TGroupBoxAccess(Control).Caption
  else if Control is TcxCustomGroupBox then
    Result := TcxGroupBox(Control).Caption
  else if Control is TCustomPanel then
    Result := TPanelAccess(Control).Caption
  else
    Result := '';
end;

function TdxAutoLayoutItem.GetLabelCaption: string;
begin
  if Control is TCustomLabel then
    Result := TLabelAccess(Control).Caption
  else if Control is TCustomStaticText then
    Result := TStaticTextAccess(Control).Caption
  else if Control is TcxCustomLabel then
    Result := TcxCustomLabel(Control).Caption
  else
    Result := '';
end;

function TdxAutoLayoutItem.IsAligned: Boolean;
begin
  Result := FAlign in [alLeft, alTop, alRight, alBottom, alClient];
end;

function TdxAutoLayoutItem.IsButton: Boolean;
begin
  Result := (Control is TcxCustomButton) or (Control is TButton);
end;

function TdxAutoLayoutItem.IsGroup: Boolean;
begin
  Result := ((Control is TCustomGroupBox) or (Control is TCustomPanel) or
    (Control is TcxCustomGroupBox)) and not (Control is TcxCustomButtonGroup);  //and (TWinControl(Control).ControlCount > 0);
end;

function TdxAutoLayoutItem.IsHiddenGroup: Boolean;
begin
  if Control is TCustomGroupBox then
    Result := False
  else if Control is TCustomPanel then
    with TPanelAccess(Control) do
      Result := (BorderStyle = bsNone) and (BevelInner = bvNone) and
        (BevelOuter = bvNone) and (BevelKind = TBevelKind.bkNone)
  else if Control is TcxCustomGroupBox then
    Result := TcxGroupBoxAccess(Control).Style.BorderStyle = ebsNone
  else
    Result := True;
end;

function TdxAutoLayoutItem.IsLabel: Boolean;
begin
  Result := (Control is TCustomLabel) or (Control is TCustomStaticText) or
    (Control is TcxCustomLabel);
end;

function TdxAutoLayoutItem.IsSeparator: Boolean;
begin
  Result := Control is TBevel;
  if Result then
    Result := TBevel(Control).Shape in [bsTopLine, bsBottomLine, bsLeftLine, bsRightLine];
end;

function TdxAutoLayoutItem.IsSplitter: Boolean;
begin
  Result := Control is TSplitter;
  if not Result then
    Result := Control is TcxSplitter;
end;

function TdxAutoLayoutItem.IsPageControl: Boolean;
begin
  Result := (Control is TPageControl) or (Control is TcxPageControl);
end;

function TdxAutoLayoutItem.IsStandaloneLabel: Boolean;
begin
  Result := IsLabel and not FLabeled;
end;

procedure TdxAutoLayoutItem.SetLabelInfo(ALabel: TControl);

  function GetCaptionLayout(ALabel: TControl): TdxCaptionLayout;
  begin
    if ALabel.BoundsRect.Right <= Control.BoundsRect.Left then
      Result := clLeft
    else
      if ALabel.BoundsRect.Left >= Control.BoundsRect.Right then
        Result := clRight
      else
        if ALabel.BoundsRect.Bottom <= Control.BoundsRect.Top then
          Result := clTop
        else
          Result := clBottom;
  end;

  procedure CorrectBounds;
  begin
    FLabeledBoundsRect.Left := Min(FBoundsRect.Left, ALabel.BoundsRect.Left);
    FLabeledBoundsRect.Top := Min(FBoundsRect.Top, ALabel.BoundsRect.Top);
    FLabeledBoundsRect.Right := Max(FBoundsRect.Right, ALabel.BoundsRect.Right);
    FLabeledBoundsRect.Bottom := Max(FBoundsRect.Bottom, ALabel.BoundsRect.Bottom);
  end;

begin
  FCaption := TControlAccess(ALabel).Caption;
  FCaptionLayout := GetCaptionLayout(ALabel);
  FLabeled := True;
  CorrectBounds;
end;

function TdxAutoLayoutItem.GetFocusControl: TWinControl;
begin
  if Control is TcxCustomLabel then
    Result := TcxLabelAccess(Control).FocusControl
  else
    if Control is TCustomLabel then
      Result := TLabelAccess(Control).FocusControl
    else
      if Control is TCustomStaticText then
        Result := TStaticTextAccess(Control).FocusControl
      else
        Result := nil;
end;

procedure TdxAutoLayoutItem.Apply(ANeedSetAligns: Boolean = True);
begin
  if Labeled and (LayoutControlItem is TdxLayoutItem) then
  begin
    TdxLayoutItem(LayoutControlItem).Caption := Caption;
    TdxLayoutItem(LayoutControlItem).CaptionOptions.Layout := CaptionLayout;
  end;
  if ANeedSetAligns then
    SetAligns;
end;

procedure TdxAutoLayoutItem.SetAligns;
begin
  if LayoutControlItem.IsRoot then Exit;
  if Abs(LayoutHorizontalCenter - cxRectCenter(Control.BoundsRect).X) < CenterAlignmentAccuracy then
    LayoutControlItem.AlignHorz := ahCenter
  else
    LayoutControlItem.AlignHorz := AlignHorzs[akLeft in FAnchors, akRight in FAnchors];
  LayoutControlItem.AlignVert := AlignVerts[akTop in FAnchors, akBottom in FAnchors];
  if not (csAncestor in FControl.ComponentState) and (IsPageControl or IsGroup or IsSplitter or IsSeparator) then
    FreeAndNil(FControl);
end;

{ TdxAutoLayoutItemList }

function TdxAutoLayoutItemList.CanConvert(ASide: TcxBorder): Boolean;
var
  I, J: Integer;
begin
  Result := Count > 0;
  if Result then
  begin
    if ASide in [bTop, bBottom] then
    begin
      for I := 0 to Count - 1 do
        for J := I + 1 to Count - 1 do
          if (Items[I].BoundsRect.Top > Items[J].BoundsRect.Bottom) or
            (Items[J].BoundsRect.Top > Items[I].BoundsRect.Bottom) then
          begin
            Result := False;
            Break;
          end;
    end
    else
    begin
      for I := 0 to Count - 1 do
        for J := I + 1 to Count - 1 do
          if (Items[I].BoundsRect.Left > Items[J].BoundsRect.Right) or
            (Items[J].BoundsRect.Left > Items[I].BoundsRect.Right) then
          begin
            Result := False;
            Break;
          end;
    end;
  end;
end;

function TdxAutoLayoutItemList.First: TdxAutoLayoutItem;
begin
  Result := Items[0];
end;

procedure TdxAutoLayoutItemList.FreeItems;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Free;
  Clear;
end;

function TdxAutoLayoutItemList.GetItem(Index: Integer): TdxAutoLayoutItem;
begin
  Result := TdxAutoLayoutItem(inherited Items[Index]);
end;

function TdxAutoLayoutItemList.IsButtons: Boolean;
var
  I: Integer;
begin
  Result := Count > 0;
  if Result then
    for I := 0 to Count - 1 do
      if not Items[I].IsButton then
      begin
        Result := False;
        Break;
      end;
end;

function TdxAutoLayoutItemList.Last: TdxAutoLayoutItem;
begin
  Result := Items[Count - 1];
end;

{ TdxAutoLayoutOwnedItemList }

constructor TdxAutoLayoutOwnedItemList.Create(AAutoLayout: TdxAutoLayout);
begin
  inherited Create;
  FAutoLayout := AAutoLayout;
end;

function TdxAutoLayoutOwnedItemList.Add(AControl: TControl): TdxAutoLayoutItem;
begin
  Result := TdxAutoLayoutItem.Create(FAutoLayout, AControl);
  inherited Add(Result);
end;

function TdxAutoLayoutOwnedItemList.AddAlignedItems(AItem: TdxAutoLayoutItem): TdxAutoLayoutItem;
begin
  Result := Add(AItem.Control);
  Result.FLabeled := AItem.Labeled;
  Result.FCaption := AItem.Caption;
  Result.FCaptionLayout := AItem.CaptionLayout;
end;

procedure TdxAutoLayoutOwnedItemList.DeleteItems(AItems: TdxAutoLayoutItemList);
var
  I, AIndex: Integer;
begin
  for I := 0 to AItems.Count - 1 do
  begin
    AIndex := IndexOf(AItems[I]);
    if AIndex >= 0 then
      Delete(AIndex);
  end;
end;

function TdxAutoLayoutOwnedItemList.Remove(AControl: TControl): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
    if AControl = Items[I].Control then
    begin
      Extract(I);
      Result := True;
      Break;
    end;
end;

procedure TdxAutoLayoutOwnedItemList.RemoveItems(AItems: TdxAutoLayoutItemList);
var
  I, AIndex: Integer;
begin
  for I := 0 to AItems.Count - 1 do
  begin
    AIndex := IndexOf(AItems[I]);
    if AIndex >= 0 then
      Extract(AIndex);
  end;
end;

procedure TdxAutoLayoutOwnedItemList.Extract(AIndex: Integer);
begin
  Items[AIndex].Free;
  Delete(AIndex);
end;

function TdxAutoLayoutOwnedItemList.Find(AControl: TControl): TdxAutoLayoutItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if AControl = Items[I].Control then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TdxAutoLayoutOwnedItemList.GetItem(Index: Integer): TdxAutoLayoutItem;
begin
  Result := TdxAutoLayoutItem(inherited Items[Index]);
end;

{ TdxAutoLayoutHelper }

constructor TdxAutoLayoutHelper.Create(AOwner: TdxAutoLayout);
begin
  inherited Create;
  FOwner := AOwner;
  FAlignedItems := TdxAutoLayoutOwnedItemList.Create(Owner);
  FItems := TdxAutoLayoutOwnedItemList.Create(Owner);
end;

destructor TdxAutoLayoutHelper.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FAlignedItems);
  inherited Destroy;
end;

procedure TdxAutoLayoutHelper.Execute(AContainer: TWinControl;
  ARoot: TdxLayoutGroup);
begin
  Initialize(AContainer, ARoot);
  PopulateItems;
  ProcessLabels;
  DoExecute;
end;

procedure TdxAutoLayoutHelper.CheckRootDirection(AItems: TdxAutoLayoutItemList; ADirection: TdxLayoutDirection);

  function GetRootDirection: TdxLayoutDirection;
  begin
    if ADirection = ldHorizontal then
      Result := ldVertical
    else
      Result := ldHorizontal;
  end;

begin
  FRoot := NewGroup(AItems);
  Root.LayoutDirection := GetRootDirection;
end;

procedure TdxAutoLayoutHelper.ConvertGroup(AItems: TdxAutoLayoutItemList;
  ASide: TcxBorder; const APreviousGroupBounds: TRect; out AGroupBounds: TRect);

  function GetLayoutItemBounds(AItem: TdxAutoLayoutItem): TRect;
  begin
    if AItem.Labeled then
      Result := AItem.LabeledBoundsRect
    else
      Result := AItem.Control.BoundsRect;
  end;

  procedure CalculateLayoutCenter(AGroup: TdxLayoutGroup; out ABounds: TRect);
  var
    I, ACenter: Integer;
    ALayoutItemBounds: TRect;
  begin
    ALayoutItemBounds := GetLayoutItemBounds(AItems[0]);
    ABounds.Left := ALayoutItemBounds.Left;
    ABounds.Right := ALayoutItemBounds.Right;
    for I := 1 to AItems.Count - 1 do
    begin
      ALayoutItemBounds := GetLayoutItemBounds(AItems[I]);
      ABounds.Left := Min(ALayoutItemBounds.Left, ABounds.Left);
      ABounds.Right := Max(ALayoutItemBounds.Right, ABounds.Right);
    end;
    ACenter := cxRectCenter(ABounds).X;
    if AGroup.LayoutDirection = ldVertical then
      for I := 0 to AItems.Count - 1 do
        if (Abs(AItems[I].Control.Left - ABounds.Left) > BorderAlignmentAccuracy) and
          (Abs(AItems[I].Control.BoundsRect.Right - ABounds.Right) > BorderAlignmentAccuracy) then
            AItems[I].LayoutHorizontalCenter := ACenter;
  end;

  procedure AddItems;
  const
    Directions: array[TcxBorder] of TdxLayoutDirection =
      (ldVertical, ldHorizontal, ldVertical, ldHorizontal);
  var
    I: Integer;
    ACanSetAligns: Boolean;
    AGroup: TdxLayoutGroup;
  begin
    AGroup := Root;
    if (AItems.Count > 1) or not AItems[0].CanResize then
    begin
      AGroup := NewGroup(AItems);
      AGroup.LayoutDirection := Directions[ASide];
    end;
    for I := 0 to AItems.Count - 1 do
      AItems[I].CreateLayoutControlItem(AGroup);
    ACanSetAligns := AItems.CanConvert(ASide);
    CalculateLayoutCenter(AGroup, AGroupBounds);
    if (APreviousGroupBounds.Left <> AGroupBounds.Right) and
     (Abs(AGroupBounds.Left - APreviousGroupBounds.Left) > BorderAlignmentAccuracy) and
     (Abs(AGroupBounds.Right - APreviousGroupBounds.Right) > BorderAlignmentAccuracy) and
     (Abs(cxRectCenter(AGroupBounds).X - cxRectCenter(APreviousGroupBounds).X) < CenterAlignmentAccuracy) then
        AGroup.AlignHorz := ahCenter;
    for I := 0 to AItems.Count - 1 do
      AItems[I].Apply(ACanSetAligns and not AItems[I].IsSeparator);
  end;

begin
  if AItems.Count = 0 then Exit;
  if ASide in [bTop, bBottom] then
    AItems.Sort(CompareLeft)
  else
    AItems.Sort(CompareTop);
  AddItems;
  Items.RemoveItems(AItems);
end;

procedure TdxAutoLayoutHelper.DoExecute;

  procedure PopulatePageControlLayout;
  var
    ALayoutItem: TdxAutoLayoutItem;
  begin
    ALayoutItem := TdxAutoLayoutItem.Create(Owner, Container);
    try
      ALayoutItem.PopulateTabbedGroup(Root);
    finally
      ALayoutItem.Free;
    end;
  end;

  procedure SetRootAlign;
  var
    I: Integer;
    AGroupAnchors: TAnchors;
  begin
    if ItemCount = 0 then Exit;
    AGroupAnchors := [];
    for I := 0 to ItemCount - 1 do
    begin
      AGroupAnchors := AGroupAnchors + Items[I].FAnchors;
      if AGroupAnchors = [akLeft, akTop, akRight, akBottom] then
        Break;
    end;
    if akRight in AGroupAnchors then
      Root.AlignHorz := ahClient;
    if akBottom in AGroupAnchors then
      Root.AlignVert := avClient;
  end;

var
  I: Integer;
  AItem: TdxAutoLayoutItem;
begin
  SetRootAlign;
  if (Container is TPageControl) or (Container is TcxPageControl) then
    PopulatePageControlLayout
  else
  begin
    for I := ItemCount - 1 downto 0 do
    begin
      AItem := Items[I];
      if AItem.IsAligned then
      begin
        FAlignedItems.AddAlignedItems(AItem);
        Items.Remove(AItem.Control);
      end;
    end;
    GenerateLayout;
  end;
end;

procedure TdxAutoLayoutHelper.DoNonAlignedControls;
begin
  if ItemCount = 0 then Exit;
  GenerateNonAlignedLayout;
end;

procedure TdxAutoLayoutHelper.Initialize(AContainer: TWinControl;
  ARoot: TdxLayoutGroup);
begin
  FRoot := ARoot;
  FContainer := AContainer;
  FAlignedItems.Clear;
  FItems.Clear;
  FAligns := [];
  FNonAlignedBoundsRect := Container.ClientRect;
end;

procedure TdxAutoLayoutHelper.PopulateItems;
var
  I: Integer;
begin
  for I := 0 to Container.ControlCount - 1 do
    if CanExport(Container.Controls[I]) then
      Items.Add(Container.Controls[I]);
end;

procedure TdxAutoLayoutHelper.PopulateItemsAtRect(AItems: TdxAutoLayoutItemList;
  const R: TRect);
var
  I: Integer;
begin
  AItems.Clear;
  for I := 0 to ItemCount - 1 do
    if cxRectIntersect(R, Items[I].BoundsRect) then
      AItems.Add(Items[I]);
end;

procedure TdxAutoLayoutHelper.GenerateAligned(AAlign: TAlign);
var
  L: TdxAutoLayoutItemList;
  I: Integer;
begin
  if not (AAlign in FAligns) then Exit;
  L := TdxAutoLayoutItemList.Create;
  try
    for I := 0 to FAlignedItems.Count - 1 do
      if FAlignedItems[I].FAlign = AAlign then
        L.Add(FAlignedItems[I]);
    if L.Count > 1 then
      case AAlign of
        alTop: L.Sort(CompareTop);
        alBottom: L.Sort(CompareBottom);
        alLeft: L.Sort(CompareLeft);
        alRight: L.Sort(CompareRight);
      end;
    if L.Count > 0 then
      case AAlign of
        alTop: FNonAlignedBoundsRect.Top := L.Last.BoundsRect.Bottom;
        alBottom: FNonAlignedBoundsRect.Bottom := L.First.BoundsRect.Top;
        alLeft: FNonAlignedBoundsRect.Left := L.Last.BoundsRect.Right;
        alRight: FNonAlignedBoundsRect.Right := L.First.BoundsRect.Left;
      end;
    for I := 0 to L.Count - 1 do
      L[I].CreateLayoutControlItem(Root);
  finally
    L.Free;
  end;
end;

procedure TdxAutoLayoutHelper.GenerateLayout;
var
  I: Integer;
  AFirstLevelRoot, ASecondLevelRoot: TdxLayoutGroup;
begin
  for I := 0 to FAlignedItems.Count - 1 do
    Include(FAligns, FAlignedItems[I].Control.Align);
  GenerateAligned(alTop);
  AFirstLevelRoot := FRoot;
  if FAligns * [alTop, alBottom] <> [] then
    MakeNewRoot;
  Root.LayoutDirection := ldHorizontal;
  GenerateAligned(alLeft);
  ASecondLevelRoot := FRoot;
  if FAligns * [alLeft, alRight] <> [] then
  begin
    MakeNewRoot;
    Root.LayoutDirection := ldVertical;
  end;
  GenerateAligned(alClient);
  DoNonAlignedControls;
  if FAligns * [alLeft, alRight] <> [] then
    FRoot := ASecondLevelRoot;
  GenerateAligned(alRight);
  if FAligns * [alTop, alBottom] <> [] then
    FRoot := AFirstLevelRoot;
  GenerateAligned(alBottom);
  for I := 0 to FAlignedItems.Count - 1 do
    FAlignedItems[I].Apply;
end;

procedure TdxAutoLayoutHelper.GenerateNonAlignedLayout;
var
  ASides: array[TcxBorder] of TdxAutoLayoutItemList;
  I, AMostSuitableSide: TcxBorder;
  ATestSides: TcxBorders;
  ASideOrder: TdxSideItemList;

  function GetSideWeight(ASide: TcxBorder): Integer;
  begin
    Result := 0;
    if not (ASide in ATestSides) then Exit;
    Result := Result or ((ASides[ASide].Count and $FF) shl 3);
    Result := Result or (Ord(ASides[ASide].IsButtons) shl 2);
    Result := Result or (Ord(ASide) and $3);
  end;

  function GetMostSuitableSide: TcxBorder;
  var
    I: TcxBorder;
    AWeight: Integer;
  begin
    Result := bLeft;
    AWeight := GetSideWeight(Result);
    for I := Succ(Low(TcxBorder)) to High(TcxBorder) do
      if GetSideWeight(I) > AWeight then
      begin
        Result := I;
        AWeight := GetSideWeight(Result);
      end;
  end;

  procedure ProcessUngroupedItems(ASide: TcxBorder; const APreviousGroupBounds: TRect);
  var
    AUnroupedItems: TdxAutoLayoutItemList;
    I: Integer;
    AGroupBounds: TRect;
  begin
    AUnroupedItems := TdxAutoLayoutItemList.Create;
    try
      for I := 0 to ItemCount - 1 do
        AUnroupedItems.Add(Items[I]);
      ConvertGroup(AUnroupedItems, ASide, APreviousGroupBounds, AGroupBounds);
    finally
      AUnroupedItems.Free;
    end;
  end;

var
  APreviousGroupBounds: TRect;
begin
  AMostSuitableSide := bTop;
  ASideOrder := TdxSideItemList.Create(Self);
  try
    for I := Low(TcxBorder) to High(TcxBorder) do
      ASides[I] := TdxAutoLayoutItemList.Create;
    try
      while ItemCount > 0 do
      begin
        ATestSides := [];
        for I := Low(TcxBorder) to High(TcxBorder) do
        begin
          GetSideItemGroup(ASides[I], I);
          if ASides[I].CanConvert(I) then
            Include(ATestSides, I);
        end;
        if ATestSides = [] then Break;
        AMostSuitableSide := GetMostSuitableSide;
        ASideOrder.Add(ASides[AMostSuitableSide], AMostSuitableSide);
        if ItemCount = 1 then
        begin
          ASides[AMostSuitableSide].Clear;
          ASides[AMostSuitableSide].Add(Items[0]);
          ASideOrder.Add(ASides[AMostSuitableSide], AMostSuitableSide);
        end;
      end;
    finally
      for I := Low(TcxBorder) to High(TcxBorder) do
        ASides[I].Free;
    end;
    ASideOrder.BuildLayout(APreviousGroupBounds);
  finally
    ASideOrder.Free;
  end;
  ProcessUngroupedItems(AMostSuitableSide, APreviousGroupBounds);
end;

procedure TdxAutoLayoutHelper.GetSideItemGroup(AItems: TdxAutoLayoutItemList;
  ASide: TcxBorder);

  procedure InitParams(out AIncX, AIncY, AEdge: Integer; out R: TRect);
  const
    DeltaX: array[TcxBorder] of Integer = (1, 0, -1, 0);
    DeltaY: array[TcxBorder] of Integer = (0, 1, 0, -1);
  var
    APos: Integer;
  begin
    R := UpdateUtmostItems;
    AIncX := DeltaX[ASide];
    AIncY := DeltaY[ASide];
    case ASide of
      bTop:
        begin
          APos := FTopmostItem.BoundsRect.Bottom;
          AEdge := R.Bottom;
          R := cxRect(R.Left, APos, R.Right, APos + 1);
        end;
      bBottom:
        begin
          APos := FBottommostItem.BoundsRect.Top;
          AEdge := R.Top;
          R := cxRect(R.Left, APos, R.Right, APos + 1);
        end;
      bLeft:
        begin
          APos := FLeftmostItem.BoundsRect.Right;
          AEdge := R.Right;
          R := cxRect(APos, R.Top, APos + 1, R.Bottom);
        end;
      bRight:
        begin
          APos := FRightmostItem.BoundsRect.Left;
          AEdge := R.Left;
          R := cxRect(APos, R.Top, APos + 1, R.Bottom);
        end;
    end;
  end;

  function GetItemsBounds(const ANonItemsBounds: TRect): TRect;
  begin
    with ANonItemsBounds do
    begin
      case ASide of
        bTop:
          Result := cxRect(Left, FTopmostItem.BoundsRect.Top, Right, Top);
        bBottom:
          Result := cxRect(Left, Top, Right, FBottommostItem.BoundsRect.Bottom);
        bLeft:
          Result := cxRect(FLeftmostItem.BoundsRect.Left, Top, Left, Bottom);
        else //bRight:
          Result := cxRect(Left + 1, Top, FRightmostItem.BoundsRect.Right, Bottom);
      end;
    end;
  end;

  function Done(const R: TRect; AEdge: Integer): Boolean;
  begin
    case ASide of
      bTop:
        Result := R.Top > AEdge;
      bBottom:
        Result := R.Top < AEdge;
      bLeft:
        Result := R.Left > AEdge;
      else //bRight:
        Result := R.Left < AEdge;
    end;
  end;

var
  AIncX, AIncY, AEdge: Integer;
  R: TRect;
begin
  AItems.Clear;
  if ItemCount = 0 then Exit;
  InitParams(AIncX, AIncY, AEdge, R);
  repeat
    OffsetRect(R, AIncX, AIncY);
    if not HasItemsAtRect(R) then
    begin
      PopulateItemsAtRect(AItems, GetItemsBounds(R));
      if AItems.Count > 0 then
        Exit;
    end;
  until Done(R, AEdge);
end;

function TdxAutoLayoutHelper.GetFocusControl(AItem: TdxAutoLayoutItem): TWinControl;
var
  I, X, Y, DX, DY: Integer;
  AControlX, AControlY: TControl;
begin
  Result := AItem.GetFocusControl;
  if (Result = nil) and Owner.SmartFindFocusControl then
  begin
    AControlX := nil;
    DX := MaxInt;
    Y := (AItem.BoundsRect.Top + AItem.BoundsRect.Bottom) div 2;
    for I := AItem.BoundsRect.Right + 1 to Container.ClientWidth do
    begin
      AControlX := GetControlAtPos(I, Y);
      if AControlX <> nil  then
      begin
        if not (AControlX is TWinControl) then
          AControlX := nil
        else
          DX := AControlX.Left - AItem.BoundsRect.Right;
        Break;
      end;
    end;
    if (AControlX <> nil) and (DX < Owner.FocusControlDistanceX) then
    begin
      Result := TWinControl(AControlX);
      Exit;
    end;
    AControlY := nil;
    DY := MaxInt;
    X := (AItem.BoundsRect.Right + AItem.BoundsRect.Left) div 2;
    for I := AItem.BoundsRect.Bottom + 1 to Container.ClientHeight do
    begin
      AControlY := GetControlAtPos(X, I);
      if AControlY <> nil  then
      begin
        if not (AControlY is TWinControl) then
          AControlY := nil
        else
          DY := AControlY.Top - AItem.BoundsRect.Bottom;
        Break;
      end;
    end;
    if (AControlY <> nil) and (DY < Owner.FocusControlDistanceY) then
      Result := TWinControl(AControlY);
  end;
end;

function TdxAutoLayoutHelper.CanExport(AControl: TControl): Boolean;
begin
  Result := (AControl <> Owner.Control) and (AControl <> Container) and
    not (csNoDesignVisible in AControl.ControlStyle) and
    not (csSubComponent in AControl.ComponentStyle) and (AControl.ClassName <> 'TdxRibbon');
end;

function TdxAutoLayoutHelper.FindBottommostItem: TdxAutoLayoutItem;
var
  I: Integer;
begin
  Result := Items[0];
  for I := 1 to ItemCount - 1 do
    if Items[I].BoundsRect.Bottom > Result.BoundsRect.Bottom then
      Result := Items[I];
end;

function TdxAutoLayoutHelper.FindLeftmostItem: TdxAutoLayoutItem;
var
  I: Integer;
begin
  Result := Items[0];
  for I := 1 to ItemCount - 1 do
    if Items[I].BoundsRect.Left < Result.BoundsRect.Left then
      Result := Items[I];
end;

function TdxAutoLayoutHelper.FindRightmostItem: TdxAutoLayoutItem;
var
  I: Integer;
begin
  Result := Items[0];
  for I := 1 to ItemCount - 1 do
    if Items[I].BoundsRect.Right > Result.BoundsRect.Right then
      Result := Items[I];
end;

function TdxAutoLayoutHelper.FindTopmostItem: TdxAutoLayoutItem;
var
  I: Integer;
begin
  Result := Items[0];
  for I := 1 to ItemCount - 1 do
    if Items[I].BoundsRect.Top < Result.BoundsRect.Top then
      Result := Items[I];
end;

function TdxAutoLayoutHelper.GetControlAtPos(X, Y: Integer): TControl;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Container.ControlCount - 1 do
    if (Owner.Control <> Container.Controls[I]) and
      cxRectPtIn(Container.Controls[I].BoundsRect, X, Y) then
    begin
      Result := Container.Controls[I];
      Break;
    end;
end;

function TdxAutoLayoutHelper.GetItemCount: Integer;
begin
  Result := Items.Count;
end;

function TdxAutoLayoutHelper.HasItemsAtRect(const R: TRect): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to ItemCount - 1 do
    if cxRectIntersect(R, Items[I].BoundsRect) then
    begin
      Result := True;
      Break;
    end;
end;

procedure TdxAutoLayoutHelper.MakeNewRoot;
begin
  FRoot := FRoot.CreateGroup;
  FRoot.AlignHorz := ahClient;
  FRoot.AlignVert := avClient;
  FRoot.Hidden := True;
end;

function TdxAutoLayoutHelper.NewGroup(AItems: TdxAutoLayoutItemList): TdxLayoutGroup;
var
  AAlignHorz: TdxLayoutAlignHorz;
  AAlignVert: TdxLayoutAlignVert;
begin
  Result := Root.CreateGroup;
  ProcessAnchors(AItems, AAlignHorz, AAlignVert);
  Result.AlignHorz := AAlignHorz;
  Result.AlignVert := AAlignVert;
  Result.Hidden := True;
end;

procedure TdxAutoLayoutHelper.ProcessAnchors(AItems: TdxAutoLayoutItemList;
   out AAlignHorz: TdxLayoutAlignHorz; out AAlignVert: TdxLayoutAlignVert);
var
  I: Integer;
  AGroupAnchors: TAnchors;
begin
  if AItems.Count = 0 then Exit;
  AGroupAnchors := [];
  for I := 0 to AItems.Count - 1 do
  begin
    AGroupAnchors := AGroupAnchors + AItems.Items[I].FAnchors;
    if AGroupAnchors = [akLeft, akTop, akRight, akBottom] then
      Break;
  end;
  AAlignHorz := AlignHorzs[akLeft in AGroupAnchors, akRight in AGroupAnchors];
  AAlignVert := AlignVerts[akTop in AGroupAnchors, akBottom in AGroupAnchors];
end;

procedure TdxAutoLayoutHelper.ProcessLabels;
var
  I: Integer;
  AControl: TWinControl;
  AItem: TdxAutoLayoutItem;
begin
  for I := ItemCount - 1 downto 0 do
    if Items[I].IsLabel then
    begin
      AControl := GetFocusControl(Items[I]);
      if AControl <> nil then
      begin
        AItem := Items.Find(AControl);
        if AItem <> nil then
        begin
          AItem.SetLabelInfo(Items[I].Control);
          Items.Extract(I);
        end;
      end;
    end;
end;

function TdxAutoLayoutHelper.UpdateUtmostItems: TRect;
begin
  FTopmostItem := FindTopmostItem;
  FBottommostItem := FindBottommostItem;
  FLeftmostItem := FindLeftmostItem;
  FRightmostItem := FindRightmostItem;
  Result := cxRect(FLeftmostItem.BoundsRect.Left, FTopmostItem.BoundsRect.Top,
    FRightmostItem.BoundsRect.Right, FBottommostItem.BoundsRect.Bottom);
end;

{ TdxAutoLayout }

constructor TdxAutoLayout.Create(AControl: TdxCustomLayoutControl);
begin
  inherited Create;
  FControl := AControl;
  FConvertPageControls := True;
  FSmartFindFocusControl := True;
  FFocusControlDistanceX := 48;
  FFocusControlDistanceY := 16;
  FUseLabeledItems := True;
end;

procedure TdxAutoLayout.DoExecute(AContainer: TWinControl; ARecursive: Boolean = True);
begin
  FRecursive := ARecursive;
  with TdxAutoLayoutHelper.Create(Self) do
  try
    Control.BeginUpdate;
    try
      Execute(AContainer, Control.Items);
      CorrectAlign(Control.Items);
    finally
      Control.EndUpdate;
    end;
  finally
    Free;
  end;
end;

procedure TdxAutoLayout.Execute(AContainer: TWinControl; ARecursive: Boolean = True);
begin
  if AContainer = nil then
    Exit;
  AContainer.DisableAlign;
  try
    DoExecute(AContainer, ARecursive);
  finally
    AContainer.EnableAlign;
  end;
end;

procedure TdxAutoLayout.CorrectAlign(AGroup: TdxLayoutGroup);

  procedure DoCorrectAlign(AItem: TdxCustomLayoutItem);
  begin
    if (AItem.AlignHorz = TdxCustomLayoutItemAccess(AItem).GetParentManagedAlignHorz) and
      not ((AItem.AlignHorz = ahClient) and (AItem.Parent.LayoutDirection = ldVertical)) then
      AItem.AlignHorz := ahParentManaged;
    if (AItem.AlignVert = TdxCustomLayoutItemAccess(AItem).GetParentManagedAlignVert) and
      not ((AItem.AlignVert = avClient) and (AItem.Parent.LayoutDirection = ldHorizontal)) then
      AItem.AlignVert := avParentManaged;
  end;

var
  I: Integer;
begin
  for I := 0 to AGroup.Count -1 do
  begin
    if AGroup[I] is TdxLayoutGroup then
      CorrectAlign(TdxLayoutGroup(AGroup[I]));
    DoCorrectAlign(AGroup[I]);
  end;
end;

procedure ImportLayout(ALayoutControl: TdxLayoutControl; AContainer: TWinControl;
  AConvertPageControls, ASmartFindFocusControl, AUseLabeledItems: Boolean;
  AFocusControlDistanceX, AFocusControlDistanceY: Integer); overload;
var
  AAutoLayout: TdxAutoLayout;
  ASaveCursor: TCursor;
begin
  if AContainer = nil then Exit;
  AAutoLayout := TdxAutoLayout.Create(ALayoutControl);
  try
    AAutoLayout.ConvertPageControls := AConvertPageControls;
    AAutoLayout.SmartFindFocusControl := ASmartFindFocusControl;
    AAutoLayout.UseLabeledItems := AUseLabeledItems;
    AAutoLayout.FocusControlDistanceX := AFocusControlDistanceX;
    AAutoLayout.FocusControlDistanceY := AFocusControlDistancey;
    ASaveCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      AAutoLayout.Execute(AContainer, True);
    finally
      Screen.Cursor := ASaveCursor;
    end;
  finally
    AAutoLayout.Free;
  end;
  ALayoutControl.AutoSize := True
end;

procedure ImportLayout(ALayoutControl: TdxLayoutControl; ADefaultImport: Boolean = False); overload;

  function GetRoot: TWinControl;
  begin
    Result := ALayoutControl.Owner as TWinControl;
  end;

var
  AImportForm: TfmImport;
begin
  if ALayoutControl = nil then Exit;
  AImportForm := TfmImport.Create(nil);
  try
    AImportForm.PopulateContainersList(ALayoutControl, GetRoot);
    if ADefaultImport or (AImportForm.ShowModal = mrOk) then
      ImportLayout(ALayoutControl, AImportForm.GetImportContainer,
        AImportForm.cbConvertPageControls.Checked, AImportForm.cbAssociate.Checked, AImportForm.cbUseLabeledItems.Checked,
        AImportForm.seDeltaX.Value, AImportForm.seDeltaY.Value);
  finally
    AImportForm.Free;
  end;
end;

{ TfmImport }

function TfmImport.GetImportContainer: TWinControl;
begin
  if (cbContainers.ItemIndex >= 0) and (cbContainers.ItemIndex < cbContainers.Properties.Items.Count) then
    Result := cbContainers.Properties.Items.Objects[cbContainers.ItemIndex] as TWinControl
  else
    Result := nil;
end;

procedure TfmImport.PopulateContainersList(ALayoutControl: TdxCustomLayoutControl; AParentContainer: TWinControl);

  function InternalPopulateContainersList(AParentContainer: TWinControl; AList: TStrings; AInsertionIndex: Integer): Boolean;
  var
    I: Integer;
  begin
    Result := CanExport(ALayoutControl, AParentContainer) and ((csAcceptsControls in AParentContainer.ControlStyle) or (AParentContainer is TPageControl));
    if Result then
    begin
      AList.InsertObject(AInsertionIndex, AParentContainer.Name, AParentContainer);
      AInsertionIndex := AList.Count;
      for I := 0 to AParentContainer.ControlCount - 1 do
        if AParentContainer.Controls[I] is TWinControl then
          if InternalPopulateContainersList(TWinControl(AParentContainer.Controls[I]), AList, AInsertionIndex) then
            Inc(AInsertionIndex);
    end;
  end;

begin
  InternalPopulateContainersList(AParentContainer, cbContainers.Properties.Items, 0);

  if cbContainers.Properties.Items.Count > 0 then
    cbContainers.ItemIndex := 0
  else
    btnImport.Enabled := False;
end;

function TfmImport.CanExport(ALayoutControl: TdxCustomLayoutControl; AContainer: TWinControl): Boolean;
begin
  Result := (AContainer <> ALayoutControl) and
    ((ALayoutControl.Parent = AContainer) or not IsParent(AContainer, ALayoutControl)) and
    not (csNoDesignVisible in AContainer.ControlStyle) and not (csSubComponent in AContainer.ComponentStyle);
end;

procedure TfmImport.cbAssociateClick(Sender: TObject);
begin
  seDeltaX.Enabled := cbAssociate.Checked;
  seDeltaY.Enabled := cbAssociate.Checked;
end;

procedure TfmImport.FormCreate(Sender: TObject);
begin
  seDeltaX.Value := 48;
  seDeltaY.Value := 16;
end;

end.

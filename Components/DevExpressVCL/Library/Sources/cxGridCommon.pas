{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
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
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit cxGridCommon;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, TypInfo, Classes, Graphics, Controls, ComCtrls,
  cxClasses, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxTreeView, cxGeometry;


const
  ScxGridPrefixName = 'TcxGrid';

  cxGridCellTextOffset               = 2;
  cxGridEditOffset                   = 1;

  cxGridDesignViewBorderColor        = 3577330;
  cxGridDesignViewColor              = 9295359;
  cxGridDesignViewHotColor           = 6862841;
  cxGridDesignSelectedBorderColor    = 11504744;
  cxGridDesignSelectedColor          = 14991773;
  cxGridDesignSelectedHotBorderColor = clDkGray;
  cxGridDesignSelectedHotColor       = 14065255;

type
  TcxGridCellState = (gcsNone, gcsSelected, gcsPressed);

  TcxGridClassEnumeratorProc = procedure(AClass: TClass) of object;

  { custom change }

  TcxCustomGridChange = class
  private
    FControl: TcxControl;
  public
    function CanExecuteWhenLocked: Boolean; virtual;
    procedure Execute; virtual; abstract;
    function IsCompatibleWith(AChange: TcxCustomGridChange): Boolean; virtual;
    function IsCumulative: Boolean; virtual;
    function IsEqual(AChange: TcxCustomGridChange): Boolean; virtual;
    function IsLockable: Boolean; virtual;
    property Control: TcxControl read FControl write FControl;  // TcxCustomGrid
  end;

  { custom drag open info }

  TcxCustomGridDragOpenInfo = class
  public
    function Equals(AInfo: TcxCustomGridDragOpenInfo): Boolean; reintroduce; virtual;
    procedure Run; virtual; abstract;
  end;

  { options tree view }

  TcxGridOptionsTreeViewGetTypeItemCaptionFunction = function(ATypeItem: Integer): string;
  TcxGridOptionsTreeViewItemKind = (otikCategory, otikCheckBox, otikRadioButton);

  TcxGridOptionsTreeView = class(TcxTreeView)
  private
    FItemValuesUpdateLocked: Boolean;
    FMaxID: Integer;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure CustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure GetNodeImageIndex(Sender: TObject; Node: TTreeNode);
    procedure GetNodeSelectedImageIndex(Sender: TObject; Node: TTreeNode);
  protected
    procedure CreateWnd; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;

    function AddItem(AParentID, AID: Integer; const ACaption: string;
      AKind: TcxGridOptionsTreeViewItemKind): Integer; virtual;
    procedure ClearItems;
    function FindNodeByID(AID: Integer): TTreeNode;
    function GetItemImageIndex(AKind: TcxGridOptionsTreeViewItemKind; AChecked: Boolean): Integer;
    function GetNodeID(ANode: TTreeNode): Integer;
    function GetNodeKind(ANode: TTreeNode): TcxGridOptionsTreeViewItemKind;
    function GetNodeParentID(ANode: TTreeNode): Integer;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure NodeClicked(ANode: TTreeNode); virtual;
    procedure RefreshImages;
    procedure RefreshItemImageIndexes;
    procedure ResetMaxID;

    function CalculateImagesSize: TSize; virtual;
    procedure DrawImage(ACanvas: TcxCanvas; const ARect: TRect;
      AItemKind: TcxGridOptionsTreeViewItemKind; AChecked: Boolean); virtual;
    function GetCategoryBitmap: TBitmap;
    function GetCategorySize: TSize; virtual;
    function GetCheckBoxSize: TSize; virtual;
    function GetRadioButtonSize: TSize; virtual;

    procedure AddItems; virtual; abstract;
    function IsItemChecked(AParentID, AID: Integer): Boolean; virtual; abstract;
    procedure ItemClicked(AParentID, AID: Integer); virtual; abstract;

    property ItemValuesUpdateLocked: Boolean read FItemValuesUpdateLocked
      write FItemValuesUpdateLocked;
    property MaxID: Integer read FMaxID;
  public
    constructor Create(AOwner: TComponent); override;

    function AddCategory(AParentID, AID: Integer; const ACaption: string): Integer;
    function AddCheckBox(AParentID, AID: Integer; const ACaption: string): Integer;
    function AddRadioButton(AParentID, AID: Integer; const ACaption: string): Integer;
    procedure AddRadioButtons(AParentID: Integer; ATypeInfo: PTypeInfo;
      AGetTypeItemCaption: TcxGridOptionsTreeViewGetTypeItemCaptionFunction);

    procedure RefreshItems;
    procedure RefreshItemValues;
  end;

function GetValidName(AComponent: TComponent; const AName: string; AIsBaseName: Boolean = False): string;

procedure GetCellTextAreaSize(var ATextSize: Integer; AScaleFactor: TdxScaleFactor);
function GridCellStateToButtonState(ACellState: TcxGridCellState): TcxButtonState;

implementation

{$R cxGrid.res}

uses
  Math, Forms, SysUtils, cxLibraryConsts;

{ TcxCustomGridChange }

function TcxCustomGridChange.CanExecuteWhenLocked: Boolean;
begin
  Result:= True;
end;

function TcxCustomGridChange.IsCompatibleWith(AChange: TcxCustomGridChange): Boolean;
begin
  Result := False;
end;

function TcxCustomGridChange.IsCumulative: Boolean;
begin
  Result := True;
end;

function TcxCustomGridChange.IsEqual(AChange: TcxCustomGridChange): Boolean;
begin
  Result := ClassType = AChange.ClassType;
end;

function TcxCustomGridChange.IsLockable: Boolean;
begin
  Result := True;
end;

{ TcxCustomGridDragOpenInfo }

function TcxCustomGridDragOpenInfo.Equals(AInfo: TcxCustomGridDragOpenInfo): Boolean;
begin
  Result := ClassType = AInfo.ClassType;
end;

{ TcxGridOptionsTreeView }

const
  OptionsTreeViewMaxInternalID = 99;

type
  TOptionTreeNodeData = class
  public
    ID: Integer;
    Kind: TcxGridOptionsTreeViewItemKind;
    constructor Create(AID: Integer; AKind: TcxGridOptionsTreeViewItemKind);
  end;

constructor TOptionTreeNodeData.Create(AID: Integer; AKind: TcxGridOptionsTreeViewItemKind);
begin
  inherited Create;
  ID := AID;
  Kind := AKind;
end;

constructor TcxGridOptionsTreeView.Create(AOwner: TComponent);
begin
  inherited;
  ReadOnly := True;
  ShowButtons := False;
  ShowRoot := False;
  Images := TImageList.Create(Self);
  OnCustomDrawItem := CustomDrawItem;
  OnGetImageIndex := GetNodeImageIndex;
  OnGetSelectedIndex := GetNodeSelectedImageIndex;
  ResetMaxID;
end;

procedure TcxGridOptionsTreeView.WMDestroy(var Message: TWMDestroy);
begin
  ClearItems;
  inherited;
end;

procedure TcxGridOptionsTreeView.CustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  R: TRect;
begin
  if (csRecreating in Sender.ControlState) or (Node.Data = nil) then
    Exit;

  // to fix the bug in VCL tree view
  Sender.Canvas.Font.Name := 'System';
  Sender.Canvas.Font.Name := Font.Name;

  if GetNodeKind(Node) = otikCategory then
  begin
    if Node.Level = 0 then
    begin
      R := Node.DisplayRect(False);
      R.Left := Node.DisplayRect(True).Left;
      Sender.Canvas.Font.Color := LookAndFeelPainter.DefaultGridOptionsTreeViewCategoryTextColor(cdsSelected in State);
      Sender.Canvas.Brush.Color := LookAndFeelPainter.DefaultGridOptionsTreeViewCategoryColor(cdsSelected in State);
      Sender.Canvas.FillRect(R);
    end;
    Sender.Canvas.Font.Style := [fsBold];
  end;
end;

procedure TcxGridOptionsTreeView.GetNodeImageIndex(Sender: TObject; Node: TTreeNode);
begin
  if GetNodeKind(Node) = otikCategory then
    Node.ImageIndex := GetItemImageIndex(otikCategory, Node.Expanded);
end;

procedure TcxGridOptionsTreeView.GetNodeSelectedImageIndex(Sender: TObject; Node: TTreeNode);
begin
  if GetNodeKind(Node) = otikCategory then
    Node.SelectedIndex := GetItemImageIndex(otikCategory, Node.Expanded);
end;

procedure TcxGridOptionsTreeView.CreateWnd;
begin
  inherited;
  RefreshImages;
  RefreshItems;
end;

procedure TcxGridOptionsTreeView.LookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
  inherited;
  if not FIsCreating then
    RefreshImages;
end;

function TcxGridOptionsTreeView.AddItem(AParentID, AID: Integer;
  const ACaption: string; AKind: TcxGridOptionsTreeViewItemKind): Integer;
var
  AParent: TTreeNode;
begin
  if AParentID = -1 then
    AParent := nil
  else
    AParent := FindNodeByID(AParentID);
  if AID = -1 then
  begin
    Inc(FMaxID);
    AID := FMaxID;
  end
  else
    FMaxID := Max(FMaxID, AID);
  Result := AID;
  Items.AddChildObject(AParent, ACaption, TOptionTreeNodeData.Create(AID, AKind));
end;

procedure TcxGridOptionsTreeView.ClearItems;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    TOptionTreeNodeData(Items[I].Data).Free;
  Items.Clear;
end;

function TcxGridOptionsTreeView.FindNodeByID(AID: Integer): TTreeNode;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
  begin
    Result := Items[I];
    if GetNodeID(Result) = AID then Exit;
  end;
  Result := nil;
end;

function TcxGridOptionsTreeView.GetItemImageIndex(AKind: TcxGridOptionsTreeViewItemKind;
  AChecked: Boolean): Integer;
begin
  Result := 2 * Ord(AKind) + Ord(AChecked);
end;

function TcxGridOptionsTreeView.GetNodeID(ANode: TTreeNode): Integer;
begin
  Result := TOptionTreeNodeData(ANode.Data).ID;
end;

function TcxGridOptionsTreeView.GetNodeKind(ANode: TTreeNode): TcxGridOptionsTreeViewItemKind;
begin
  Result := TOptionTreeNodeData(ANode.Data).Kind;
end;

function TcxGridOptionsTreeView.GetNodeParentID(ANode: TTreeNode): Integer;
begin
  if ANode.Parent = nil then
    Result := -1
  else
    Result := GetNodeID(ANode.Parent);
end;

procedure TcxGridOptionsTreeView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_SPACE) and (Selected <> nil) then
    NodeClicked(Selected);
end;

procedure TcxGridOptionsTreeView.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ANode: TTreeNode;
begin
  if Button = mbLeft then
  begin
    ANode := GetNodeAt(X, Y);
    if (ANode <> nil) and (htOnItem in GetHitTestInfoAt(X, Y)) then
      NodeClicked(ANode);
  end;
  inherited;
end;

procedure TcxGridOptionsTreeView.NodeClicked(ANode: TTreeNode);
begin
  ItemValuesUpdateLocked := True;
  try
    ItemClicked(GetNodeParentID(ANode), GetNodeID(ANode));
  finally
    ItemValuesUpdateLocked := False;
  end;
  RefreshItemValues;
end;

procedure TcxGridOptionsTreeView.RefreshImages;
var
  AChecked: Boolean;
  AImage: TcxBitmap;
  AItemKind: TcxGridOptionsTreeViewItemKind;
begin
  if IsDestroying then Exit;

  Images.Clear;
  with CalculateImagesSize do
  begin
    Images.Width := cx;
    Images.Height := cy;
  end;

  AImage := TcxBitmap.CreateSize(Images.Width, Images.Height, pf32bit);
  try
    for AItemKind := Low(AItemKind) to High(AItemKind) do
      for AChecked := Low(AChecked) to High(AChecked) do
      begin
        AImage.cxCanvas.FillRect(AImage.ClientRect, GetBackgroundColor);
        DrawImage(AImage.cxCanvas, AImage.ClientRect, AItemKind, AChecked);
        Images.Add(AImage, nil);
      end;
  finally
    AImage.Free;
  end;
end;

procedure TcxGridOptionsTreeView.RefreshItemImageIndexes;
var
  I: Integer;
  ANode: TTreeNode;
begin
  Items.BeginUpdate;
  try
    for I := 0 to Items.Count - 1 do
    begin
      ANode := Items[I];
      ANode.ImageIndex := GetItemImageIndex(GetNodeKind(ANode),
        IsItemChecked(GetNodeParentID(ANode), GetNodeID(ANode)));
      ANode.SelectedIndex := ANode.ImageIndex;
    end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TcxGridOptionsTreeView.ResetMaxID;
begin
  FMaxID := OptionsTreeViewMaxInternalID;
end;

function TcxGridOptionsTreeView.CalculateImagesSize: TSize;
begin
  Result := GetCategorySize;
  Result.cx := Max(Result.cx, Max(GetCheckBoxSize.cx, GetRadioButtonSize.cx));
  Result.cy := Max(Result.cy, Max(GetCheckBoxSize.cy, GetRadioButtonSize.cy));
end;

procedure TcxGridOptionsTreeView.DrawImage(ACanvas: TcxCanvas; const ARect: TRect;
  AItemKind: TcxGridOptionsTreeViewItemKind; AChecked: Boolean);
var
  R: TRect;
  B: TBitmap;
begin
  R := ARect;
  case AItemKind of
    otikCategory:
      begin
        R := cxRectCenter(R, GetCategorySize);
        B := GetCategoryBitmap;
        try
          if AChecked then
            ACanvas.RotateBitmap(B, raMinus90);
          ACanvas.DrawGlyph(R.Left, R.Top, B);
        finally
          B.Free;
        end;
      end;
    otikCheckBox:
      begin
        R := cxRectCenter(R, GetCheckBoxSize);
        LookAndFeelPainter.DrawScaledCheckButton(ACanvas, R, cxbsNormal, AChecked, ScaleFactor);
      end;
    otikRadioButton:
      begin
        R := cxRectCenter(R, GetRadioButtonSize);
        LookAndFeelPainter.DrawScaledRadioButton(ACanvas, R.Left, R.Top, cxbsNormal,
          AChecked, False, ACanvas.Brush.Color, ScaleFactor);
      end;
  end;
end;

function TcxGridOptionsTreeView.GetCategoryBitmap: TBitmap;
begin
  Result := TBitmap.Create;
  Result.LoadFromResourceName(HInstance, 'CXGRIDOPTIONSTREEVIEWCATEGORYBITMAP');
end;

function TcxGridOptionsTreeView.GetCategorySize: TSize;
begin
  with GetCategoryBitmap do
    try
      Result.cx := Width;
      Result.cy := Height;
    finally
      Free;
    end;
end;

function TcxGridOptionsTreeView.GetCheckBoxSize: TSize;
begin
  Result := LookAndFeelPainter.ScaledCheckButtonSize(ScaleFactor);
end;

function TcxGridOptionsTreeView.GetRadioButtonSize: TSize;
begin
  Result := LookAndFeelPainter.ScaledRadioButtonSize(ScaleFactor);
end;

function TcxGridOptionsTreeView.AddCategory(AParentID, AID: Integer;
  const ACaption: string): Integer;
begin
  Result := AddItem(AParentID, AID, ACaption, otikCategory);
end;

function TcxGridOptionsTreeView.AddCheckBox(AParentID, AID: Integer;
  const ACaption: string): Integer;
begin
  Result := AddItem(AParentID, AID, ACaption, otikCheckBox);
end;

function TcxGridOptionsTreeView.AddRadioButton(AParentID, AID: Integer;
  const ACaption: string): Integer;
begin
  Result := AddItem(AParentID, AID, ACaption, otikRadioButton);
end;

procedure TcxGridOptionsTreeView.AddRadioButtons(AParentID: Integer;
  ATypeInfo: PTypeInfo; AGetTypeItemCaption: TcxGridOptionsTreeViewGetTypeItemCaptionFunction);
var
  ATypeData: PTypeData;
  I: Integer;
begin
  if ATypeInfo.Kind <> tkEnumeration then Exit;
  ATypeData := GetTypeData(ATypeInfo);
  for I := ATypeData.MinValue to ATypeData.MaxValue do
    AddRadioButton(AParentID, I, AGetTypeItemCaption(I));
end;

procedure TcxGridOptionsTreeView.RefreshItems;
begin
  Items.BeginUpdate;
  try
    ClearItems;
    ResetMaxID;
    AddItems;
    RefreshItemValues;
    FullExpand;
    TopItem := Items.GetFirstNode;
  finally
    Items.EndUpdate;
  end;
end;

procedure TcxGridOptionsTreeView.RefreshItemValues;
begin
  if not ItemValuesUpdateLocked then
    RefreshItemImageIndexes;
end;

{ functions }

function GetValidName(AComponent: TComponent; const AName: string; AIsBaseName: Boolean = False): string;
begin
  Result := cxClasses.GetValidName(AComponent, AName, AIsBaseName);
end;

procedure GetCellTextAreaSize(var ATextSize: Integer; AScaleFactor: TdxScaleFactor);
begin
  Inc(ATextSize, 2 * AScaleFactor.Apply(cxGridCellTextOffset));
end;

function GridCellStateToButtonState(ACellState: TcxGridCellState): TcxButtonState;
const
  ButtonStates: array[TcxGridCellState] of TcxButtonState =
    (cxbsNormal, cxbsHot, cxbsPressed);
begin
  Result := ButtonStates[ACellState];
end;

initialization
  Screen.Cursors[crcxGridSelectRow] := LoadCursor(HInstance, 'CX_GRIDSELECTROWCURSOR');

end.

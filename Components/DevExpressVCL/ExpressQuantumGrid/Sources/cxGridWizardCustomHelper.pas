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

unit cxGridWizardCustomHelper;

{$I cxVer.inc}

interface

uses
  cxGridStructureNavigator,
  Windows, Classes, Forms, SysUtils, DB, dxCoreClasses, cxClasses, cxGraphics, cxGrid, cxGridDBDataDefinitions,
  cxGridLevel, cxGridCustomView, dxCustomWizardControl, dxGDIPlusClasses, cxControls, Graphics, cxDBData,
  dxServerModeData, cxEdit, cxGridCustomTableView;

type
  TcxGridWizardGridViewType = (gvtUnbound, gvtDB, gvtServerMode);

  { IcxGridWizardHelperDataSetFieldsSupport }

  IcxGridWizardHelperDataSetFieldsSupport = interface
  ['{A4AB41E3-9DA8-4479-A55B-B2A73D3D6EA6}']
    function GetDataSetFields: TFields;
  end;

  { IcxGridWizardHelperDBDataControllerSupport }

  IcxGridWizardHelperDBDataControllerSupport = interface
  ['{7100F9AE-E34B-47F0-BD41-E57BE4736068}']
    function GetDataController: TcxDBDataController;
  end;

  { TcxGridWizardGridViewOptionsData }

  TcxGridWizardGridViewOptionsData = record
    Appending: Boolean;
    Deleting: Boolean;
    Editing: Boolean;
    Inserting: Boolean;
  end;

  { TcxGridWizardCustomHelper }

  TcxComponentFilterProc = function (AComponent: TComponent): Boolean;

  TcxGridWizardCustomHelperClass = class of TcxGridWizardCustomHelper;
  TcxGridWizardCustomHelper = class(TcxIUnknownObject)
  private
    FGrid: TcxCustomGrid;
    FGridView: TcxCustomGridView;
    procedure AssignData(ASourceView, ATargetView: TcxCustomGridView);
  protected
    FOriginalGridViewOptionsData: TcxGridWizardGridViewOptionsData;

    function AddNewItem: Integer; virtual; abstract;
    function CanSaveData: Boolean; virtual;
    procedure CheckComponentNames(AView: TcxCustomGridView); virtual; abstract;
    procedure CheckGridLevelName(AGrid: TcxCustomGrid; ALevel: TcxGridLevel);
    procedure SaveGridViewData(AView: TcxCustomGridView); virtual;

    function GetGridView: TcxCustomGridView;
    function GetItemCaption(Index: Integer): string; virtual; abstract;
    function GetItemFieldName(Index: Integer): string; virtual;
    function GetItemProperties(Index: Integer): TcxCustomEditPropertiesClass; virtual;
    function GetItemsCount: Integer; virtual; abstract;
    function GetItemsVisibleCount: Integer; virtual; abstract;
    function GetItemVisible(Index: Integer): Boolean; virtual; abstract;
    function GetPageClassCount: Integer; virtual; abstract;
    function GetPageClasses(Index: Integer): TClass; virtual; abstract;
    procedure SetItemCaption(Index: Integer; const AValue: string); virtual; abstract;
    procedure SetItemFieldName(Index: Integer; const AValue: string); virtual;
    procedure SetItemProperties(Index: Integer; AValue: TcxCustomEditPropertiesClass); virtual;
    procedure SetItemVisible(Index: Integer; const AValue: Boolean); virtual; abstract;
  public
    constructor Create(AGrid: TcxCustomGrid); virtual;
    destructor Destroy; override;

    procedure AddItem(const AFieldName: string; AProperties: TcxCustomEditPropertiesClass = nil); overload;
    procedure AddItem(const AFieldName: string; const ACaption: string; AProperties: TcxCustomEditPropertiesClass = nil); overload;
    procedure Assign(ASourceView: TcxCustomGridView); virtual;
    procedure ChangeItemIndex(const AOldIndex, ANewIndex: Integer); virtual; abstract;
    procedure DeleteItem(const AFieldName: string); overload; virtual;
    procedure DeleteItem(const AItemIndex: Integer); overload; virtual; abstract;
    function GetDefaultItemCaption: string; virtual; abstract;
    function GetItemIndexByFieldName(const AFieldName: string): Integer; virtual;
    procedure InitializeEditingMode(AEditingView, APreviewView: TcxCustomGridView); virtual;
    procedure PreparePreview(APreviewView: TcxCustomGridView); virtual;

    procedure CorrectCustomizationFormContent(ACustomizationForm: TForm); virtual;
    procedure PrepareForCustomization; virtual;
    procedure RestoreAfterCustomization; virtual;

    function CreateGridView(AGrid: TcxCustomGrid; ALevel: TcxGridLevel): TcxCustomGridView; virtual;
    class function CanBeMasterView: Boolean; virtual;
    class function GetGridViewClass: TcxCustomGridViewClass; virtual;
    class function GetGridViewType: TcxGridWizardGridViewType; virtual;
    procedure SaveGridView(ASelectedView: TcxCustomGridView; AGrid: TcxCustomGrid); virtual;
    procedure SaveGridViewAndDeleteDetailed(ASelectedView: TcxCustomGridView; AGrid: TcxCustomGrid); virtual;
    procedure SaveGridViewAsDetail(ASelectedView, AMasterView: TcxCustomGridView; AGrid: TcxCustomGrid); virtual;

    procedure PopulateComponents(AClassType: TComponentClass; AFilterProc: TcxComponentFilterProc; AList: TStrings);
    procedure RestoreGridViewOptionsData; virtual; abstract;
    procedure SaveGridViewOptionsData; virtual; abstract;

    property ItemCaption[Index: Integer]: string read GetItemCaption write SetItemCaption;
    property ItemFieldName[Index: Integer]: string read GetItemFieldName write SetItemFieldName;
    property ItemProperties[Index: Integer]: TcxCustomEditPropertiesClass read GetItemProperties write SetItemProperties;
    property ItemVisible[Index: Integer]: Boolean read GetItemVisible write SetItemVisible;
    property ItemsCount: Integer read GetItemsCount;
    property ItemsVisibleCount: Integer read GetItemsVisibleCount;
    property GridView: TcxCustomGridView read GetGridView;
    property PageClassCount: Integer read GetPageClassCount;
    property PageClasses[Index: Integer]: TClass read GetPageClasses;
  end;

  { TcxGridWizardCustomTableLikeViewHelper }

  TcxGridWizardCustomTableLikeViewHelper = class(TcxGridWizardCustomHelper)
  private
    function GetGridView: TcxCustomGridTableView;
    function GetItem(Index: Integer): TcxCustomGridTableItem;
  protected
    function AddNewItem: Integer; override;
    function GetItemCaption(Index: Integer): string; override;
    function GetItemProperties(Index: Integer): TcxCustomEditPropertiesClass; override;
    function GetItemsCount: Integer; override;
    function GetItemsVisibleCount: Integer; override;
    function GetItemVisible(Index: Integer): Boolean; override;
    procedure SetItemCaption(Index: Integer; const AValue: string); override;
    procedure SetItemProperties(Index: Integer; AValue: TcxCustomEditPropertiesClass); override;
    procedure SetItemVisible(Index: Integer; const AValue: Boolean); override;
  public
    procedure ChangeItemIndex(const AOldIndex, ANewIndex: Integer); override;
    procedure DeleteItem(const AItemIndex: Integer); overload; override;

    property GridView: TcxCustomGridTableView read GetGridView;
    property Items[Index: Integer]: TcxCustomGridTableItem read GetItem;
  end;

  { TcxGridWizardHelperInfo }

  TcxGridWizardHelperInfo = class
  private
    FGlyph: TcxBitmap32;
    FHelperClass: TcxGridWizardCustomHelperClass;
    function GetGridViewClass: TcxCustomGridViewClass;
  public
    constructor Create(AHelperClass: TcxGridWizardCustomHelperClass; AInstance: HINST);
    destructor Destroy; override;

    property Glyph: TcxBitmap32 read FGlyph write FGlyph;
    property GridViewClass: TcxCustomGridViewClass read GetGridViewClass;
    property HelperClass: TcxGridWizardCustomHelperClass read FHelperClass write FHelperClass;
  end;

  { TcxGridWizardHelperInfoList }

  TcxGridWizardHelperInfoList = class(TcxObjectList)
  private
    function GetItem(Index: Integer): TcxGridWizardHelperInfo;
  public
    procedure Add(AHelperClass: TcxGridWizardCustomHelperClass; AInstance: HINST);
    function GetItemIndexBy(AGridViewClass: TClass): Integer;
    function GetHelperClassBy(AGridViewClass: TClass): TcxGridWizardCustomHelperClass;

    property Items[Index: Integer]: TcxGridWizardHelperInfo read GetItem; default;
  end;

function cxGridWizardGetGridViewGroup(AGridViewClass: TcxCustomGridViewClass): TcxGridWizardGridViewType;
function cxGridWizardHelperInfoList: TcxGridWizardHelperInfoList;
implementation

uses
  StrUtils;

var
  FHelperInfoList: TcxGridWizardHelperInfoList;

function cxGridWizardGetGridViewGroup(AGridViewClass: TcxCustomGridViewClass): TcxGridWizardGridViewType;
begin
  if Pos('DB', AGridViewClass.ClassName) > 0 then
    Result := gvtDB
  else
    if Pos('ServerMode', AGridViewClass.ClassName) > 0 then
      Result := gvtServerMode
    else
      Result := gvtUnbound;
end;

function cxGridWizardHelperInfoList: TcxGridWizardHelperInfoList;
begin
  if FHelperInfoList = nil then
    FHelperInfoList := TcxGridWizardHelperInfoList.Create;
  Result := FHelperInfoList;
end;

{ TcxGridWizardCustomHelper }

constructor TcxGridWizardCustomHelper.Create(AGrid: TcxCustomGrid);
begin
  inherited Create;
  FGrid := AGrid;
  FGridView := GetGridViewClass.Create(nil);
end;

destructor TcxGridWizardCustomHelper.Destroy;
begin
  FreeAndNil(FGridView);
  inherited Destroy;
end;

procedure TcxGridWizardCustomHelper.AddItem(const AFieldName: string; AProperties: TcxCustomEditPropertiesClass);
var
  AIndex: Integer;
begin
  AIndex := AddNewItem;
  ItemFieldName[AIndex] := AFieldName;
  ItemProperties[AIndex] := AProperties;
end;

procedure TcxGridWizardCustomHelper.AddItem(const AFieldName, ACaption: string; AProperties: TcxCustomEditPropertiesClass);
var
  AIndex: Integer;
begin
  AIndex := AddNewItem;
  ItemCaption[AIndex] := ACaption;
  ItemFieldName[AIndex] := AFieldName;
  ItemProperties[AIndex] := AProperties;
end;

procedure TcxGridWizardCustomHelper.Assign(ASourceView: TcxCustomGridView);
begin
  GridView.Assign(ASourceView);
  if CanSaveData then
    AssignData(ASourceView, GridView);
end;

procedure TcxGridWizardCustomHelper.DeleteItem(const AFieldName: string);
begin
  // do nothing
end;

function TcxGridWizardCustomHelper.GetItemIndexByFieldName(const AFieldName: string): Integer;
begin
  Result := -1;
end;

procedure TcxGridWizardCustomHelper.InitializeEditingMode(AEditingView, APreviewView: TcxCustomGridView);
begin
  Assign(AEditingView);
  GridView.Name := AEditingView.Name;

  if CanSaveData then
  begin
    APreviewView.Assign(GridView);
    AssignData(GridView, APreviewView);
  end;
end;

procedure TcxGridWizardCustomHelper.PreparePreview(APreviewView: TcxCustomGridView);
begin
  APreviewView.Assign(GridView);
end;

function TcxGridWizardCustomHelper.GetItemProperties(Index: Integer): TcxCustomEditPropertiesClass;
begin
  Result := nil;
end;

procedure TcxGridWizardCustomHelper.CorrectCustomizationFormContent(ACustomizationForm: TForm);
begin
  //do nothing
end;

procedure TcxGridWizardCustomHelper.PrepareForCustomization;
begin
  //do nothing
end;

procedure TcxGridWizardCustomHelper.RestoreAfterCustomization;
begin
  //do nothing
end;

function TcxGridWizardCustomHelper.CreateGridView(AGrid: TcxCustomGrid; ALevel: TcxGridLevel): TcxCustomGridView;
begin
  Result := AGrid.CreateView(GetGridViewClass);
  if Result.Name = '' then
    Result.Name := GenViewName(AGrid, Result);
  ALevel.GridView := Result;
end;

class function TcxGridWizardCustomHelper.CanBeMasterView: Boolean;
begin
  Result := False;
end;

class function TcxGridWizardCustomHelper.GetGridViewClass: TcxCustomGridViewClass;
begin
  Result := nil; // CBuilder
end;

class function TcxGridWizardCustomHelper.GetGridViewType: TcxGridWizardGridViewType;
begin
  Result := cxGridWizardGetGridViewGroup(GetGridViewClass);
end;

procedure TcxGridWizardCustomHelper.SaveGridView(ASelectedView: TcxCustomGridView; AGrid: TcxCustomGrid);
var
  AActualView: TcxCustomGridView;
  ALevel: TcxGridLevel;
begin
  if ASelectedView = nil then
    AActualView := CreateGridView(AGrid, AGrid.Levels.Add)
  else
    AActualView := ASelectedView;

  AActualView.Assign(GridView);
  ALevel := AActualView.Level as TcxGridLevel;
  CheckGridLevelName(AGrid, ALevel);
  CheckComponentNames(AActualView);
  SaveGridViewData(AActualView);
  AGrid.ActiveLevel := ALevel;
end;

procedure TcxGridWizardCustomHelper.SaveGridViewAndDeleteDetailed(ASelectedView: TcxCustomGridView; AGrid: TcxCustomGrid);
var
  ANewLevel, APrevLevel: TcxGridLevel;
begin
  ASelectedView.Assign(GridView);
  APrevLevel := ASelectedView.Level as TcxGridLevel;
  ANewLevel := AGrid.Levels.Add;
  CheckGridLevelName(AGrid, ANewLevel);
  CheckComponentNames(ASelectedView);
  SaveGridViewData(ASelectedView);
  ANewLevel.GridView := ASelectedView;
  APrevLevel.Free;
  AGrid.ActiveLevel := ANewLevel;
end;

procedure TcxGridWizardCustomHelper.SaveGridViewAsDetail(ASelectedView, AMasterView: TcxCustomGridView; AGrid: TcxCustomGrid);
var
  AActualView: TcxCustomGridView;
  AActualLevel, AMasterLevel: TcxGridLevel;
begin
  if AMasterView.Level = nil then
    AGrid.Levels.Add.GridView := AMasterView;
  AMasterLevel := AMasterView.Level as TcxGridLevel;
  CheckGridLevelName(AGrid, AMasterLevel);

  if ASelectedView = nil then
  begin
    AActualView := CreateGridView(AGrid, AMasterLevel.Add);
    AActualView.Assign(GridView);
  end
  else
  begin
    AActualView := ASelectedView;
    AActualView.Assign(GridView);
    if (AActualView.Level = nil) or (TcxGridLevel(AActualView.Level).Parent <> AMasterLevel) then
    begin
      AActualView.Level.Free;
      AMasterLevel.Add.GridView := AActualView;
    end;
  end;
  AActualLevel := AActualView.Level as TcxGridLevel;
  CheckGridLevelName(AGrid, AActualLevel);
  CheckComponentNames(AActualView);
  SaveGridViewData(AActualView);
  AGrid.ActiveLevel := AActualLevel;
end;

procedure TcxGridWizardCustomHelper.PopulateComponents(
  AClassType: TComponentClass; AFilterProc: TcxComponentFilterProc; AList: TStrings);

  procedure PopulateOwnersComponents(AOwner: TComponent);
  var
    AComponent: TComponent;
    I: Integer;
  begin
    for I := 0 to AOwner.ComponentCount - 1 do
    begin
      AComponent := AOwner.Components[I];
      if AComponent.InheritsFrom(AClassType) then
      begin
        if AFilterProc(AComponent) and (AList.IndexOfObject(AComponent) < 0) then
          AList.AddObject(IfThen(AComponent.Owner.Name <> '', AComponent.Owner.Name + '.') + AComponent.Name, AComponent);
      end;
    end;
  end;

var
  I: Integer;
begin
  AList.BeginUpdate;
  try
    AList.Clear;
    PopulateOwnersComponents(FGrid.Owner);
    for I := 0 to Screen.DataModuleCount - 1 do
      PopulateOwnersComponents(Screen.DataModules[I]);
    for I := 0 to Screen.FormCount - 1 do
      PopulateOwnersComponents(Screen.Forms[I]);
  finally
    AList.EndUpdate;
  end;
end;

function TcxGridWizardCustomHelper.CanSaveData: Boolean;
begin
  Result := False;
end;

procedure TcxGridWizardCustomHelper.CheckGridLevelName(AGrid: TcxCustomGrid; ALevel: TcxGridLevel);
begin
  if ALevel.Name = '' then
  begin
    ALevel.Name := GenLevelName(AGrid, ALevel);
    ALevel.Caption := ALevel.Name;
  end;
end;

procedure TcxGridWizardCustomHelper.SaveGridViewData(AView: TcxCustomGridView);
begin
  if CanSaveData then
    AssignData(GridView, AView);
end;

function TcxGridWizardCustomHelper.GetGridView: TcxCustomGridView;
begin
  Result := FGridView;
end;

function TcxGridWizardCustomHelper.GetItemFieldName(Index: Integer): string;
begin
  Result := EmptyStr;
end;

procedure TcxGridWizardCustomHelper.SetItemFieldName(Index: Integer; const AValue: string);
begin
  // do nothing
end;

procedure TcxGridWizardCustomHelper.SetItemProperties(Index: Integer; AValue: TcxCustomEditPropertiesClass);
begin
  //do nothing
end;

procedure TcxGridWizardCustomHelper.AssignData(ASourceView, ATargetView: TcxCustomGridView);
var
  ADataController: IcxCustomGridDataController;
begin
  if Supports(ATargetView.DataController, IcxCustomGridDataController, ADataController) then
    if ADataController.IsDataChangeable then
    begin
      ASourceView.DataController.Post;
      ADataController.AssignData(ASourceView.DataController);
    end;
end;

{ TcxGridWizardHelperInfo }

constructor TcxGridWizardHelperInfo.Create(AHelperClass: TcxGridWizardCustomHelperClass; AInstance: HINST);
var
  ABitmap: TcxBitmap32;
begin
  inherited Create;
  FGlyph := TcxBitmap32.Create;
  HelperClass := AHelperClass;
  Glyph.LoadFromResourceName(AInstance, UpperCase(GridViewClass.ClassName));
  if not cxIsVCLThemesEnabled then
  begin
    ABitmap := TcxBitmap32.CreateSize(Glyph.ClientRect);
    try
      ABitmap.cxCanvas.FillRect(ABitmap.ClientRect, clWindow);
      cxAlphaBlend(ABitmap, Glyph, ABitmap.ClientRect, ABitmap.ClientRect);
      Glyph.Assign(ABitmap);
    finally
      ABitmap.Free;
    end;
  end;
end;

destructor TcxGridWizardHelperInfo.Destroy;
begin
  FreeAndNil(FGlyph);
  inherited Destroy;
end;

function TcxGridWizardHelperInfo.GetGridViewClass: TcxCustomGridViewClass;
begin
  Result := FHelperClass.GetGridViewClass;
end;

{ TcxGridWizardHelperInfoList }

procedure TcxGridWizardHelperInfoList.Add(AHelperClass: TcxGridWizardCustomHelperClass; AInstance: HINST);
begin
  inherited Add(TcxGridWizardHelperInfo.Create(AHelperClass, AInstance));
end;

function TcxGridWizardHelperInfoList.GetItemIndexBy(AGridViewClass: TClass): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Items[I].GridViewClass = AGridViewClass then
    begin
      Result := I;
      Break;
    end;
end;

function TcxGridWizardHelperInfoList.GetHelperClassBy(AGridViewClass: TClass): TcxGridWizardCustomHelperClass;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].GridViewClass = AGridViewClass then
    begin
      Result := Items[I].HelperClass;
      Break;
    end;
end;

function TcxGridWizardHelperInfoList.GetItem(Index: Integer): TcxGridWizardHelperInfo;
begin
  Result := TcxGridWizardHelperInfo(inherited Items[Index]);
end;

{ TcxGridWizardCustomTableLikeViewHelper }

procedure TcxGridWizardCustomTableLikeViewHelper.ChangeItemIndex(const AOldIndex, ANewIndex: Integer);
begin
  Items[AOldIndex].Index := ANewIndex;
end;

procedure TcxGridWizardCustomTableLikeViewHelper.DeleteItem(const AItemIndex: Integer);
begin
  Items[AItemIndex].Free;
end;

function TcxGridWizardCustomTableLikeViewHelper.AddNewItem: Integer;
begin
  Result := CreateViewItem(GridView).Index;
end;

function TcxGridWizardCustomTableLikeViewHelper.GetItemCaption(Index: Integer): string;
begin
  Result := Items[Index].Caption;
end;

function TcxGridWizardCustomTableLikeViewHelper.GetItemProperties(Index: Integer): TcxCustomEditPropertiesClass;
begin
  Result := Items[Index].PropertiesClass;
end;

function TcxGridWizardCustomTableLikeViewHelper.GetItemsCount: Integer;
begin
  Result := GridView.ItemCount;
end;

function TcxGridWizardCustomTableLikeViewHelper.GetItemsVisibleCount: Integer;
begin
  Result := GridView.VisibleItemCount;
end;

function TcxGridWizardCustomTableLikeViewHelper.GetItemVisible(Index: Integer): Boolean;
begin
  Result := Items[Index].Visible;
end;

procedure TcxGridWizardCustomTableLikeViewHelper.SetItemCaption(Index: Integer; const AValue: string);
begin
  Items[Index].Caption := AValue;
end;

procedure TcxGridWizardCustomTableLikeViewHelper.SetItemProperties(Index: Integer; AValue: TcxCustomEditPropertiesClass);
begin
  Items[Index].PropertiesClass := AValue;
end;

procedure TcxGridWizardCustomTableLikeViewHelper.SetItemVisible(Index: Integer; const AValue: Boolean);
begin
  Items[Index].Visible := AValue;
end;

function TcxGridWizardCustomTableLikeViewHelper.GetGridView: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(inherited GridView);
end;

function TcxGridWizardCustomTableLikeViewHelper.GetItem(Index: Integer): TcxCustomGridTableItem;
begin
  Result := GridView.Items[Index];
end;

initialization

finalization
  FreeAndNil(FHelperInfoList);

end.



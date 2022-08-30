{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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

unit dxDBSparkline;

{$I cxVer.inc}

interface

uses
  Types, Variants, Windows, DB, Classes, SysUtils, Graphics, Contnrs, Controls, Messages,
  dxCore, dxCoreGraphics, dxCoreClasses, cxGraphics, cxGeometry, dxGDIPlusApi, dxGDIPlusClasses, cxDBEdit,
  cxContainer, cxCustomData, cxDBData, cxDataStorage, cxDataUtils, cxEdit, cxEditConsts, dxSparkline;


type
  { TdxLookupSparklineItemDataBinding }

  TdxLookupSparklineItemDataBinding = class(TdxSparklineItemDataBinding)
  strict private
    FFieldName: string;
    function GetDataSource: TDataSource;
    procedure SetFieldName(const AValue: string);
  protected
    procedure Initialize; override;
    function IsBindingActive: Boolean; override;
  public
    procedure Assign(Source: TPersistent); override;

    property DataSource: TDataSource read GetDataSource;
  published
    property FieldName: string read FFieldName write SetFieldName;
  end;

  { TdxLookupSparklineCategories }

  TdxLookupSparklineCategories = class(TdxSparklineCategories)
  strict private
    function GetDataBinding: TdxLookupSparklineItemDataBinding;
    procedure SetDataBinding(AValue: TdxLookupSparklineItemDataBinding);
  published
    property DataBinding: TdxLookupSparklineItemDataBinding read GetDataBinding write SetDataBinding;
  end;

  { TdxLookupSparklineSeries }

  TdxLookupSparklineSeries = class(TdxSparklineSeries)
  strict private
    function GetDataBinding: TdxLookupSparklineItemDataBinding;
    procedure SetDataBinding(AValue: TdxLookupSparklineItemDataBinding);
  published
    property DataBinding: TdxLookupSparklineItemDataBinding read GetDataBinding write SetDataBinding;
  end;

  { TdxLookupSparklineSeriesCollection }

  TdxLookupSparklineSeriesCollection = class(TdxSparklineSeriesCollection)
  strict private
    function GetItem(AIndex: Integer): TdxLookupSparklineSeries;
    procedure SetItem(AIndex: Integer; AValue: TdxLookupSparklineSeries);
  public
    function Add: TdxLookupSparklineSeries;

    property Items[Index: Integer]: TdxLookupSparklineSeries read GetItem write SetItem; default;
  end;

  { TdxLookupSparklineProperties }

  TdxLookupSparklineProperties = class(TdxSparklineProperties)
  strict private
    FDetailBinding: TdxLookupSparklineItemDataBinding;
    function GetCategories: TdxLookupSparklineCategories;
    function GetDataController: TcxDBDataController;
    function GetLookupDataSource: TDataSource;
    function GetLookupKeyFieldName: string;
    function GetSeries: TdxLookupSparklineSeriesCollection;
    procedure SetCategories(AValue: TdxLookupSparklineCategories);
    procedure SetLookupDataSource(AValue: TDataSource);
    procedure SetLookupKeyFieldName(const AValue: string);
    procedure SetSeries(AValue: TdxLookupSparklineSeriesCollection);
  protected
    function CreateCategories: TdxSparklineCategories; override;
    function CreateDataController: TcxCustomDataController; override;
    function CreateSeriesCollection: TdxSparklineSeriesCollection; override;
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    function GetItemsDataBinidingClass: TdxSparklineItemDataBindingClass; override;
    function GetSeriesClass: TdxSparklineSeriesClass; override;
    function IsActive: Boolean; override;
    procedure LoadRecords(const AEditValue: TcxEditValue; AList: TList); override;
    procedure MakeGroups;

    property DetailBinding: TdxLookupSparklineItemDataBinding read FDetailBinding write FDetailBinding;
  public
    constructor Create(AOwner: TPersistent); override;

    class function GetContainerClass: TcxContainerClass; override;
    procedure DataChanged; override;

    property DataController: TcxDBDataController read GetDataController;
  published
    property LookupDataSource: TDataSource read GetLookupDataSource write SetLookupDataSource;
    property LookupKeyFieldName: string read GetLookupKeyFieldName write SetLookupKeyFieldName;
    property Categories: TdxLookupSparklineCategories read GetCategories write SetCategories;
    property Series: TdxLookupSparklineSeriesCollection read GetSeries write SetSeries;
  end;

  { TdxCustomLookupSparklineEdit }

  TdxCustomLookupSparklineEdit = class(TdxCustomSparklineEdit)
  strict private
    function GetActiveProperties: TdxLookupSparklineProperties;
    function GetProperties: TdxLookupSparklineProperties;
    procedure SetProperties(AValue: TdxLookupSparklineProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;

    property ActiveProperties: TdxLookupSparklineProperties  read GetActiveProperties;
  published
    property Properties: TdxLookupSparklineProperties read GetProperties  write SetProperties;
  end;

  { TdxSparklineEdit }

  TdxLookupSparklineEdit = class(TdxCustomLookupSparklineEdit)
  protected
    //
  published
    property Anchors;
    property AutoSize;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EditValue;
    property ParentColor default False;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TdxDBLookupSparklineEdit }

  TdxDBLookupSparklineEdit = class(TdxCustomLookupSparklineEdit)
  private
    function GetDataBinding: TcxDBEditDataBinding;
    procedure SetDataBinding(AValue: TcxDBEditDataBinding);

    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  published
    property DataBinding: TcxDBEditDataBinding read GetDataBinding write SetDataBinding;
    property Anchors;
    property AutoSize;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor default False;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TdxDBSparklineEdit }

  TdxDBSparklineEdit = class(TdxCustomSparklineEdit)
  private
    function GetDataBinding: TcxDBEditDataBinding;
    procedure SetDataBinding(AValue: TcxDBEditDataBinding);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  published
    property DataBinding: TcxDBEditDataBinding read GetDataBinding write SetDataBinding;
    property Anchors;
    property AutoSize;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor default False;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TcxEditRepositoryLookupSparklineItem }

  TcxEditRepositoryLookupSparklineItem = class(TcxEditRepositoryItem)
  private
    function GetProperties: TdxLookupSparklineProperties;
    procedure SetProperties(Value: TdxLookupSparklineProperties);
  public
    class function GetEditPropertiesClass: TcxCustomEditPropertiesClass; override;
  published
    property Properties: TdxLookupSparklineProperties read GetProperties write SetProperties;
  end;

implementation

uses
  cxVariants;

type
  { TdxLookSparklineDataController }

  TdxLookSparklineDataController = class(TcxDBDataController)
  strict private
    FSparkline: TdxLookupSparklineProperties;
  protected
    procedure UpdateControl(AInfo: TcxUpdateControlInfo); override;
  public
    constructor Create(AOwner: TdxLookupSparklineProperties); reintroduce; overload;
    function GetItem(Index: Integer): TObject; override;
    function GetItemID(AItem: TObject): Integer; override;
    procedure UpdateData; override;
    procedure UpdateItemIndexes; override;

    property Sparkline: TdxLookupSparklineProperties read FSparkline;
  end;

{ TdxLookSparklineDataController }

constructor TdxLookSparklineDataController.Create(AOwner: TdxLookupSparklineProperties);
begin
  FSparkline := AOwner;
  inherited Create(TComponent(Sparkline.Owner));
end;

function TdxLookSparklineDataController.GetItem(Index: Integer): TObject;
begin
  Result := Sparkline.DataBindings[Index];
end;

function TdxLookSparklineDataController.GetItemID(AItem: TObject): Integer;
begin
  Result := Sparkline.DataBindings.IndexOf(AItem);
end;

procedure TdxLookSparklineDataController.UpdateData;
begin
  Sparkline.DataChanged
end;

procedure TdxLookSparklineDataController.UpdateItemIndexes;
begin
  Sparkline.UpdateItemIndexes;
end;

procedure TdxLookSparklineDataController.UpdateControl(AInfo: TcxUpdateControlInfo);
begin
  Sparkline.UpdateControl(AInfo);
end;

{ TdxLookupSparklineItemDataBinding }

procedure TdxLookupSparklineItemDataBinding.Assign(Source: TPersistent);
begin
  if Source is TdxLookupSparklineItemDataBinding then
    FieldName := TdxLookupSparklineItemDataBinding(Source).FieldName
  else
    inherited Assign(Source);
end;

procedure TdxLookupSparklineItemDataBinding.Initialize;
begin
  if Properties.Owner <> nil then
    TcxDBDataController(DataController).ChangeFieldName(DataIndex, FieldName)
end;

function TdxLookupSparklineItemDataBinding.IsBindingActive: Boolean;
begin
  if Properties.Owner <> nil then
    Result := TcxDBDataController(DataController).GetItemByFieldName(FieldName) <> nil
  else
    Result := False;
end;

function TdxLookupSparklineItemDataBinding.GetDataSource: TDataSource;
begin
  Result := TcxDBDataController(DataController).DataSource;
end;

procedure TdxLookupSparklineItemDataBinding.SetFieldName(const AValue: string);
begin
  if FieldName <> AValue then
  begin
    FFieldName := AValue;
    Initialize;
  end;
end;

{ TdxLookupSparklineCategories }

function TdxLookupSparklineCategories.GetDataBinding: TdxLookupSparklineItemDataBinding;
begin
  Result := TdxLookupSparklineItemDataBinding(inherited DataBinding);
end;

procedure TdxLookupSparklineCategories.SetDataBinding(AValue: TdxLookupSparklineItemDataBinding);
begin
  DataBinding.Assign(AValue)
end;

{ TdxLookupSparklineSeries }

function TdxLookupSparklineSeries.GetDataBinding: TdxLookupSparklineItemDataBinding;
begin
  Result := TdxLookupSparklineItemDataBinding(inherited DataBinding);
end;

procedure TdxLookupSparklineSeries.SetDataBinding(AValue: TdxLookupSparklineItemDataBinding);
begin
  DataBinding.Assign(AValue)
end;

{ TdxLookupSparklineSeriesCollection }

function TdxLookupSparklineSeriesCollection.GetItem(AIndex: Integer): TdxLookupSparklineSeries;
begin
  Result := TdxLookupSparklineSeries(inherited Items[AIndex]);
end;

procedure TdxLookupSparklineSeriesCollection.SetItem(AIndex: Integer; AValue: TdxLookupSparklineSeries);
begin
  inherited Items[AIndex].Assign(AValue);
end;

function TdxLookupSparklineSeriesCollection.Add: TdxLookupSparklineSeries;
begin
  Result := TdxLookupSparklineSeries(inherited Add);
end;

{ TdxLookupSparklineProperties }

constructor TdxLookupSparklineProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  DetailBinding := GetItemsDataBinidingClass.Create(Self, Self, TcxIntegerValueType) as TdxLookupSparklineItemDataBinding;
end;

procedure TdxLookupSparklineProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  inherited DoAssign(AProperties);
  if AProperties is TdxLookupSparklineProperties then
  begin
    LookupDataSource := TdxLookupSparklineProperties(AProperties).LookupDataSource;
    LookupKeyFieldName := TdxLookupSparklineProperties(AProperties).LookupKeyFieldName;
  end;
end;

class function TdxLookupSparklineProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TdxLookupSparklineEdit;
end;

procedure TdxLookupSparklineProperties.DataChanged;
begin
  inherited DataChanged;
  MakeGroups;
end;

function TdxLookupSparklineProperties.CreateCategories: TdxSparklineCategories;
begin
  Result := TdxLookupSparklineCategories.Create(Self);
end;

function TdxLookupSparklineProperties.CreateDataController: TcxCustomDataController;
begin
  Result := TdxLookSparklineDataController.Create(Self);
end;

function TdxLookupSparklineProperties.CreateSeriesCollection: TdxSparklineSeriesCollection;
begin
  Result := TdxLookupSparklineSeriesCollection.Create(Self);
end;

function TdxLookupSparklineProperties.GetItemsDataBinidingClass: TdxSparklineItemDataBindingClass;
begin
  Result := TdxLookupSparklineItemDataBinding;
end;

function TdxLookupSparklineProperties.GetSeriesClass: TdxSparklineSeriesClass;
begin
  Result := TdxLookupSparklineSeries;
end;

function TdxLookupSparklineProperties.IsActive: Boolean;
begin
  Result := inherited IsActive and (DataController.DataSet <> nil) and
    (LookupKeyFieldName <> ' ') and DataController.DataSet.Active;
end;

procedure TdxLookupSparklineProperties.LoadRecords(const AEditValue: TcxEditValue; AList: TList);
var
  ADataGroupIndex: TcxDataGroupIndex;
begin
  AList.Clear;
  ADataGroupIndex := DataController.Groups.GetDataGroupIndexByGroupValue(-1, AEditValue);
  if ADataGroupIndex < 0 then
    Exit;
  DataController.Groups.LoadRecordIndexes(AList, ADataGroupIndex);
end;

procedure TdxLookupSparklineProperties.MakeGroups;
begin
  if not IsActive then
    Exit;
  DataController.BeginUpdate;
  try
    FDetailBinding.FieldName := LookupKeyFieldName;
    if DetailBinding.DataIndex >= 0 then
      DataController.Groups.ChangeGrouping(DetailBinding.DataIndex, 0);
  finally
    DataController.EndUpdate;
  end;
end;

function TdxLookupSparklineProperties.GetCategories: TdxLookupSparklineCategories;
begin
  Result := inherited Categories as TdxLookupSparklineCategories;
end;

function TdxLookupSparklineProperties.GetDataController: TcxDBDataController;
begin
  Result := inherited DataController as TcxDBDataController;
end;

function TdxLookupSparklineProperties.GetLookupDataSource: TDataSource;
begin
  Result := DataController.DataSource;
end;

function TdxLookupSparklineProperties.GetLookupKeyFieldName: string;
begin
  Result := DetailBinding.FieldName;
end;

function TdxLookupSparklineProperties.GetSeries: TdxLookupSparklineSeriesCollection;
begin
  Result := inherited Series as TdxLookupSparklineSeriesCollection;
end;

procedure TdxLookupSparklineProperties.SetCategories(AValue: TdxLookupSparklineCategories);
begin
  Categories.Assign(AValue);
end;

procedure TdxLookupSparklineProperties.SetLookupDataSource(AValue: TDataSource);
begin
  DataController.DataSource := AValue;
end;

procedure TdxLookupSparklineProperties.SetLookupKeyFieldName(const AValue: string);
begin
  DetailBinding.FieldName := AValue;
end;

procedure TdxLookupSparklineProperties.SetSeries(AValue: TdxLookupSparklineSeriesCollection);
begin
  Series.Assign(AValue);
end;

{ TdxCustomLookupSparklineEdit }

class function TdxCustomLookupSparklineEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxLookupSparklineProperties;
end;

function TdxCustomLookupSparklineEdit.GetActiveProperties: TdxLookupSparklineProperties;
begin
  Result := inherited ActiveProperties as TdxLookupSparklineProperties;
end;

function TdxCustomLookupSparklineEdit.GetProperties: TdxLookupSparklineProperties;
begin
  Result := inherited Properties as TdxLookupSparklineProperties;
end;

procedure TdxCustomLookupSparklineEdit.SetProperties(AValue: TdxLookupSparklineProperties);
begin
  Properties.Assign(AValue);
end;

{ TdxDBLookupSparklineEdit }

class function TdxDBLookupSparklineEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBEditDataBinding;
end;

procedure TdxDBLookupSparklineEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

function TdxDBLookupSparklineEdit.GetDataBinding: TcxDBEditDataBinding;
begin
  Result := TcxDBEditDataBinding(inherited DataBinding);
end;

procedure TdxDBLookupSparklineEdit.SetDataBinding(AValue: TcxDBEditDataBinding);
begin
  DataBinding.Assign(AValue)
end;

{ TdxDBSparklineEdit }

class function TdxDBSparklineEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBEditDataBinding;
end;

procedure TdxDBSparklineEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

function TdxDBSparklineEdit.GetDataBinding: TcxDBEditDataBinding;
begin
  Result := TcxDBEditDataBinding(inherited DataBinding);
end;

procedure TdxDBSparklineEdit.SetDataBinding(AValue: TcxDBEditDataBinding);
begin
  DataBinding.Assign(AValue)
end;

{ TcxEditRepositorySparklineItem }

class function TcxEditRepositoryLookupSparklineItem.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxLookupSparklineProperties;
end;

function TcxEditRepositoryLookupSparklineItem.GetProperties: TdxLookupSparklineProperties;
begin
  Result := inherited Properties as TdxLookupSparklineProperties
end;

procedure TcxEditRepositoryLookupSparklineItem.SetProperties(Value: TdxLookupSparklineProperties);
begin
  inherited Properties := Value;
end;

initialization
  RegisterClasses([TdxLookupSparklineItemDataBinding, TdxLookupSparklineSeries,
    TdxLookupSparklineCategories, TdxLookupSparklineProperties, TcxEditRepositoryLookupSparklineItem]);
  GetRegisteredEditProperties.Register(TdxLookupSparklineProperties, scxSEditRepositoryLookupSparklineItem);

end.


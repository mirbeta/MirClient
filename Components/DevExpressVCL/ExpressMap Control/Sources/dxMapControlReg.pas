{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressMapControl                                        }
{                                                                    }
{           Copyright (c) 2013-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSMAPCONTROL AND ALL             }
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

unit dxMapControlReg;

interface

{$I cxVer.inc}

uses
  Forms, TypInfo, DesignIntf, DesignEditors, DesignMenus, Controls,
  cxClasses, dxCoreReg, dxCoreClasses, cxDesignWindows;

const
  dxMapControlProductName  = 'ExpressMapControl Suite';

procedure Register;

implementation

uses
  Classes, SysUtils, Menus, cxLibraryReg, cxPropEditors, cxComponentCollectionEditor,
  dxMapControl, dxMapLayer, dxMapItemFileLayer, dxMapItem,
  dxMapImageTileLayer, dxMapItemLayer, dxMapLayersEditor,
  dxCustomMapItemLayer, dxMapControlProjections, dxMapControlImageTileProvider, dxMapItemsEditor,
  dxMapControlBingMapImageryDataProvider, dxMapControlOpenStreetMapImageryDataProvider,
  dxMapInformationProvidersEditor, dxMapControlInformationProvider, dxMapControlBingMapInformationProviders;

type
  TdxMapControlComponentEditor = class(TdxComponentEditor)
  private
    function GetMapControl: TdxCustomMapControl;
  protected
    function GetProductName: string; override;
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
  public
    property MapControl: TdxCustomMapControl read GetMapControl;
  end;

  TdxMapItemFileLayerFilenameProperty = class(TcxFilenameProperty)
  protected
    function GetFilter: string; override;
  end;

  TdxMapItemsProperty = class(TcxComponentCollectionProperty)
  public
    function GetEditorClass: TcxComponentCollectionEditorClass; override;
  end;

  TdxMapLayersProperty = class(TcxComponentCollectionProperty)
  public
    function GetEditorClass: TcxComponentCollectionEditorClass; override;
  end;

  TdxMapInformationProvidersProperty = class(TcxComponentCollectionProperty)
  public
    function GetEditorClass: TcxComponentCollectionEditorClass; override;
  end;

  TdxMapControlCustomProjectionProperty = class(TClassProperty)
  protected
    function HasSubProperties: Boolean;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TdxMapControlImageTileProviderProperty = class(TClassProperty)
  protected
    function HasSubProperties: Boolean;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TdxMapControlCoordinatePatternProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TdxMapControlSelectionEditor = class(TSelectionEditor)
  protected
    ComponentsList: TStringList;
  public
    procedure AddComponent(const Name: string);
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TdxMapControlBingMapInformationProviderSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

{ TdxMapControlComponentEditor }

function TdxMapControlComponentEditor.GetProductName: string;
begin
  Result := dxMapControlProductName;
end;

procedure TdxMapControlComponentEditor.InternalExecuteVerb(AIndex: Integer);
begin
  case AIndex of
    0: ShowFormEditorClass(Designer, Component, MapControl.Layers, 'Layers', TfrmMapControlLayersEditor)
  else  // 1
    ShowFormEditorClass(Designer, Component, MapControl.InformationProviders,
      'InformationProviders', TfrmMapControlInformationProvidersEditor);
  end;
end;

function TdxMapControlComponentEditor.InternalGetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := 'Layers Editor...'
  else  // 1
    Result := 'Information Providers Editor...';
  end;
end;

function TdxMapControlComponentEditor.InternalGetVerbCount: Integer;
begin
  Result := 2;
end;

function TdxMapControlComponentEditor.GetMapControl: TdxCustomMapControl;
begin
  Result := Component as TdxCustomMapControl;
end;

{ TdxMapItemFileLayerFilenameProperty }

function TdxMapItemFileLayerFilenameProperty.GetFilter: string;
begin
  case (GetComponent(0) as TdxMapItemFileLayer).FileType of
    miftKml:
      Result := 'KML files (*.kml)|*.kml|'
  else // miftShape
    Result := 'Shapefiles (*.shp)|*.shp|'
  end;
  Result := Result + inherited GetFilter;
end;

{ TdxMapItemsProperty }

function TdxMapItemsProperty.GetEditorClass: TcxComponentCollectionEditorClass;
begin
  Result := TfrmMapItemsEditor;
end;

{ TdxMapLayersProperty }

function TdxMapLayersProperty.GetEditorClass: TcxComponentCollectionEditorClass;
begin
  Result := TfrmMapControlLayersEditor;
end;

{ TdxMapInformationProvidersProperty }

function TdxMapInformationProvidersProperty.GetEditorClass: TcxComponentCollectionEditorClass;
begin
  Result := TfrmMapControlInformationProvidersEditor;
end;

procedure UpdateObjectInspector(ADesigner: IDesigner);
var
  AComponents: IDesignerSelections;
begin
  if ADesigner <> nil then
  begin
    AComponents := CreateSelectionList;
    ADesigner.GetSelections(AComponents);
    ADesigner.ClearSelection;
    ADesigner.SetSelections(AComponents);
  end;
end;

{ TdxMapControlCustomProjectionProperty }

function TdxMapControlCustomProjectionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  Exclude(Result, paSubProperties);
  Result := Result - [paReadOnly] +
    [paValueList, paSortList, paRevertable, paVolatileSubProperties];
end;

function TdxMapControlCustomProjectionProperty.GetValue: string;
begin
  if HasSubProperties then
    Result := dxRegisteredMapProjections.GetDescriptionByClass(TdxMapControlCustomProjection(GetOrdValue).ClassType)
  else
    Result := '';
end;

procedure TdxMapControlCustomProjectionProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to dxRegisteredMapProjections.Count - 1 do
    Proc(dxRegisteredMapProjections.Descriptions[I]);
end;

procedure TdxMapControlCustomProjectionProperty.SetValue(const Value: string);
var
  AMapProjectionClass: TdxMapControlCustomProjectionClass;
  I: Integer;
begin
  AMapProjectionClass := TdxMapControlCustomProjectionClass(dxRegisteredMapProjections.FindByClassName(Value));
  if AMapProjectionClass = nil then
    AMapProjectionClass := TdxMapControlCustomProjectionClass(dxRegisteredMapProjections.FindByDescription(Value));

  ObjectInspectorCollapseProperty;
  for I := 0 to PropCount - 1 do
    TdxCustomMapItemLayer(GetComponent(I)).ProjectionClass := AMapProjectionClass;
{$IFNDEF DELPHI15}
  UpdateObjectInspector(Designer);
{$ENDIF}
  Modified;
end;

function TdxMapControlCustomProjectionProperty.HasSubProperties: Boolean;
var
  I: Integer;
begin
  for I := 0 to PropCount - 1 do
  begin
    Result := TdxCustomMapItemLayer(GetComponent(I)).Projection <> nil;
    if not Result then Exit;
  end;
  Result := True;
end;

{ TdxMapControlImageTileProviderProperty }

function TdxMapControlImageTileProviderProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if not HasSubProperties then
    Exclude(Result, paSubProperties);
  Result := Result - [paReadOnly] +
    [paValueList, paSortList, paRevertable, paVolatileSubProperties];
end;

function TdxMapControlImageTileProviderProperty.GetValue: string;
begin
  if HasSubProperties then
    Result := dxMapControlImageryDataProviders.GetDescriptionByClass(TdxMapControlImageTileProvider(GetOrdValue).ClassType)
  else
    Result := '';
end;

procedure TdxMapControlImageTileProviderProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  Proc('<None>');
  for I := 0 to dxMapControlImageryDataProviders.Count - 1 do
    Proc(dxMapControlImageryDataProviders.Descriptions[I]);
end;

procedure TdxMapControlImageTileProviderProperty.SetValue(const Value: string);
var
  AProviderClass: TdxMapControlImageTileProviderClass;
  I: Integer;
begin
  AProviderClass := TdxMapControlImageTileProviderClass(dxMapControlImageryDataProviders.FindByClassName(Value));
  if AProviderClass = nil then
    AProviderClass := TdxMapControlImageTileProviderClass(dxMapControlImageryDataProviders.FindByDescription(Value));

  ObjectInspectorCollapseProperty;
  for I := 0 to PropCount - 1 do
    TdxMapImageTileLayer(GetComponent(I)).ProviderClass := AProviderClass;
{$IFNDEF DELPHI15}
  UpdateObjectInspector(Designer);
{$ENDIF}
  Modified;
end;

function TdxMapControlImageTileProviderProperty.HasSubProperties: Boolean;
var
  I: Integer;
begin
  for I := 0 to PropCount - 1 do
  begin
    Result := TdxMapImageTileLayer(GetComponent(I)).Provider <> nil;
    if not Result then Exit;
  end;
  Result := True;
end;

{ TdxMapControlCoordinatePatternProperty }

function TdxMapControlCoordinatePatternProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueList];
end;

procedure TdxMapControlCoordinatePatternProperty.GetValues(Proc: TGetStrProc);
begin
  Proc('{D:1}°{CP}');
  Proc('{CP}{D:6}°');
  Proc('{D}°{M:2}''{CP}');
  Proc('{CP}{D}°{M:2}''');
  Proc('{D}°{M}''{S:4}''''{CP}');
  Proc('{CP}{D}°{M}''{S:4}''''');
end;

{ TdxMapControlSelectionEditor }

procedure TdxMapControlSelectionEditor.AddComponent(const Name: string);
begin
  ComponentsList.Add(Name);
end;

procedure TdxMapControlSelectionEditor.RequiresUnits(Proc: TGetStrProc);

  procedure AddUnitName(AObject: TObject);
  begin
    if AObject <> nil then
      Proc(cxGetUnitName(AObject.ClassType));
  end;

var
  AComponent: TComponent;
  I: Integer;
begin
  inherited RequiresUnits(Proc);
  Proc('dxMapControlTypes');
  ComponentsList := TStringList.Create;
  try
    Designer.GetComponentNames(GetTypeData(PTypeInfo(TdxMapImageTileLayer.ClassInfo)), AddComponent);
    for I := 0 to ComponentsList.Count - 1 do
    begin
      AComponent := Designer.GetComponent(ComponentsList[I]);
      if AComponent is TdxMapImageTileLayer then
        AddUnitName(TdxMapImageTileLayer(AComponent).Provider);
    end;
  finally
    ComponentsList.Free;
  end;
end;

{ TdxMapControlBingMapInformationProviderSelectionEditor }

procedure TdxMapControlBingMapInformationProviderSelectionEditor.RequiresUnits(
  Proc: TGetStrProc);
begin
  inherited RequiresUnits(Proc);
  Proc('dxBingMapLocationDataService');
  Proc('dxBingMapRouteDataService');
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  RegisterComponents(dxCoreLibraryProductPage, [TdxMapControl]);
  RegisterNoIcon([TdxMapLayer, TdxMapItem, TdxMapControlInformationProvider]);
  RegisterClasses([TdxMapControl, TdxCustomMapControl, TdxMapLayer, TdxMapItem,
    TdxMapImageTileLayer, TdxMapItemLayer, TdxMapItemFileLayer,
    TdxMapDot, TdxMapRectangle, TdxMapEllipse, TdxMapPolyline,
    TdxMapPolygon, TdxMapPath, TdxMapPushpin, TdxMapCustomElement,
    TdxMapControlBingMapGeoCodingDataProvider, TdxMapControlBingMapReverseGeoCodingDataProvider,
    TdxMapControlBingMapRouteDataProvider, TdxMapControlBingMapMajorRoadRouteDataProvider]);
  RegisterComponentEditor(TdxMapControl, TdxMapControlComponentEditor);
  RegisterPropertyEditor(TypeInfo(TFileName), TdxMapItemFileLayer, 'FileName', TdxMapItemFileLayerFilenameProperty);
  RegisterPropertyEditor(TypeInfo(TdxMapItems), nil, '', TdxMapItemsProperty);
  RegisterPropertyEditor(TypeInfo(TdxMapLayers), nil, '', TdxMapLayersProperty);
  RegisterPropertyEditor(TypeInfo(TdxMapControlInformationProviders), nil, '', TdxMapInformationProvidersProperty);
  RegisterPropertyEditor(TypeInfo(TdxMapControlCustomProjection), TdxCustomMapItemLayer,
    'Projection', TdxMapControlCustomProjectionProperty);
  RegisterPropertyEditor(TypeInfo(TdxMapControlImageTileProvider), TdxMapImageTileLayer,
    'Provider', TdxMapControlImageTileProviderProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxMapControlNavigationPanel, 'XCoordinateDisplayMask', TdxMapControlCoordinatePatternProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxMapControlNavigationPanel, 'YCoordinateDisplayMask', TdxMapControlCoordinatePatternProperty);
  RegisterSelectionEditor(TdxCustomMapControl, TdxMapControlSelectionEditor);
  RegisterSelectionEditor(TdxMapControlBingMapInformationProvider, TdxMapControlBingMapInformationProviderSelectionEditor);
  HideClassProperties(TdxCustomMapItemLayer, ['ProjectionClassName']);
  HideClassProperties(TdxMapImageTileLayer, ['ProviderClassName']);
end;

end.

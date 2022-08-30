{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressGaugeControl                                      }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSGAUGECONTROL AND ALL           }
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

unit dxGaugeControlReg;

interface

{$I cxVer.inc}

uses
  Classes, DesignEditors, DesignIntf, Menus,
  dxCoreReg, cxClasses, cxControls, cxDesignWindows, dxGaugeControl, dxGaugeControlSelection, dxGaugeCustomScale;

const
  dxGaugeControlProductName  = 'ExpressGaugeControl';

type
  { TdxGaugeControlComponentEditor }

  TdxGaugeControlComponentEditor = class(TdxComponentEditor)
  private
    function GetGaugeControl: TdxCustomGaugeControl;
    procedure MenuItemOnClickHandler(ASender: TObject);
  protected
    function GetProductName: string; override;
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
  public
    procedure PrepareItem(AIndex: Integer; const AItem: TDesignMenuItem); override;

    property GaugeControl: TdxCustomGaugeControl read GetGaugeControl;
  end;

  { TdxGaugeScaleCollectionProperty }

  TdxGaugeScaleCollectionProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TdxGaugeStyleNameProperty }

  TdxGaugeStyleNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(AProc: TGetStrProc); override;
  end;

  { TdxGaugeControlDesignTimeSelectionHelper }

  TdxGaugeControlDesignTimeSelectionHelper = class(TdxGaugeControlCustomSelectionHelper, IcxDesignSelectionChanged)
  private
    FDesignHelper: TcxDesignHelper;
  protected
    function GetIsControlSelected: Boolean; override;
    procedure SetSelection; override;
    procedure SelectComponent(AComponent: TComponent); override;
    procedure ShowScalesEditor; override;
    // IcxDesignSelectionChanged
    procedure DesignSelectionChanged(ASelection: TList);
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(AControl: TcxControl); override;
    destructor Destroy; override;
  end;

procedure Register;

implementation

uses
  SysUtils, cxComponentCollectionEditor, dxGaugeControlScalesEditor, dxGaugeQuantitativeScale, dxGaugeCircularScale,
  dxGaugeDigitalScale, dxGaugeLinearScale, dxGaugeDBScale;

type
  TdxCustomGaugeControlAccess = class(TdxCustomGaugeControl);
  TdxCustomGaugeControlControllerAccess = class(TdxCustomGaugeControlController);
  TdxGaugeCustomScaleAccess = class(TdxGaugeCustomScale);
  TdxGaugeCustomScaleAccessClass = class of TdxGaugeCustomScaleAccess;
  TdxGaugeQuantitativeScaleAccess = class(TdxGaugeQuantitativeScale);
  TdxGaugeScaleCollectionAccess = class(TdxGaugeScaleCollection);
  TdxGaugeControlCustomSelectionHelperAccess = class(TdxGaugeControlCustomSelectionHelper);

{ TdxGaugeControlComponentEditor }

procedure TdxGaugeControlComponentEditor.PrepareItem(AIndex: Integer; const AItem: TDesignMenuItem);

  procedure AddScaleClassesMenuItems(AScaleClasses: TList);
  var
    I: Integer;
    ANeedAddSeparator: Boolean;
    AScaleName: string;
  begin
    ANeedAddSeparator := True;
    for I := 0 to AScaleClasses.Count - 1 do
    begin
      AScaleName := TdxGaugeCustomScaleAccessClass(AScaleClasses[I]).GetScaleName;
      if TdxGaugeCustomScaleAccessClass(AScaleClasses[I]).GetScaleType <> stContainerScale then
      begin
        if ANeedAddSeparator and (Pos('DB ', AScaleName) = 1) then
        begin
          AItem.AddLine;
          ANeedAddSeparator := False;
        end;
        AItem.AddItem(AScaleName, 0, False, True, MenuItemOnClickHandler);
        AItem.Items[AItem.Count - 1].Tag := I;
      end;
    end;
  end;

var
  AScaleClasses: TList;
begin
  inherited PrepareItem(AIndex, AItem);
  if AIndex = 3 then
  begin
    AScaleClasses := TList.Create;
    try
      dxGaugeGetRegisteredScaleClasses(AScaleClasses);
      AddScaleClassesMenuItems(AScaleClasses);
    finally
      AScaleClasses.Free;
    end;
  end;
end;

function TdxGaugeControlComponentEditor.GetProductName: string;
begin
  Result := dxGaugeControlProductName;
end;

procedure TdxGaugeControlComponentEditor.InternalExecuteVerb(AIndex: Integer);
begin
  case AIndex of
    0:
      ShowFormEditorClass(Designer, GaugeControl, TfmGaugeControlScalesEditor);
    2:
      GaugeControl.AddContainer;
    else
      inherited InternalGetVerb(AIndex);
  end;
end;

function TdxGaugeControlComponentEditor.InternalGetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0:
      Result := 'Edit Scales...';
    1:
      Result := '-';
    2:
      Result := 'Add Container';
    3:
      Result := 'Add Scale';
  end;
end;

function TdxGaugeControlComponentEditor.InternalGetVerbCount: Integer;
begin
  Result := 4;
end;

function TdxGaugeControlComponentEditor.GetGaugeControl: TdxCustomGaugeControl;
begin
  Result := Component as TdxCustomGaugeControl;
end;

procedure TdxGaugeControlComponentEditor.MenuItemOnClickHandler(ASender: TObject);

  function GetScaleClass(AMenuItemIndex: Integer): TdxGaugeCustomScaleClass;
  var
    AScaleClasses: TList;
  begin
    AScaleClasses := TList.Create;
    try
      dxGaugeGetRegisteredScaleClasses(AScaleClasses);
      Result := TdxGaugeCustomScaleClass(AScaleClasses[AMenuItemIndex]);
    finally
      AScaleClasses.Free;
    end;
  end;

begin
  GaugeControl.AddScale(GetScaleClass((ASender as TMenuItem).Tag));
end;

{ TdxGaugeScaleCollectionProperty }

procedure TdxGaugeScaleCollectionProperty.Edit;
begin
  if GetComponent(0) is TdxGaugeControl then
    ShowFormEditorClass(Designer, TdxGaugeControl(GetComponent(0)), TfmGaugeControlScalesEditor);
end;

function TdxGaugeScaleCollectionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

{ TdxGaugeStyleNameProperty }

function TdxGaugeStyleNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  Result := Result - [paSubProperties, paReadOnly] +
    [paValueList, paSortList, paRevertable, paVolatileSubProperties];
end;

function TdxGaugeStyleNameProperty.GetValue: string;
begin
  Result := TdxGaugeCustomScaleAccess(GetComponent(0)).StyleName;
end;

procedure TdxGaugeStyleNameProperty.GetValues(AProc: TGetStrProc);
var
  I: Integer;
  AStyles: TStringList;
begin
  AStyles := TStringList.Create;
  try
    dxGaugeGetRegisteredStyleNames(TdxGaugeCustomScaleAccess(GetComponent(0)).GetScaleType, AStyles);
    for I := 0 to AStyles.Count - 1 do
      AProc(AStyles[I]);
  finally
    AStyles.Free;
  end;
end;

{ TdxGaugeControlDesignTimeSelectionHelper }

constructor TdxGaugeControlDesignTimeSelectionHelper.Create(AControl: TcxControl);
begin
  inherited Create(AControl);
  FDesignHelper := TcxDesignHelper.Create(AControl);
  FDesignHelper.AddSelectionChangedListener(Self);
end;

destructor TdxGaugeControlDesignTimeSelectionHelper.Destroy;
begin
  FreeAndNil(FDesignHelper);
  inherited Destroy;
end;

function TdxGaugeControlDesignTimeSelectionHelper.GetIsControlSelected: Boolean;
begin
  Result := FDesignHelper.IsObjectSelected(Control);
end;

procedure TdxGaugeControlDesignTimeSelectionHelper.SetSelection;
begin
  FDesignHelper.SetSelection(Selections);
end;

procedure TdxGaugeControlDesignTimeSelectionHelper.SelectComponent(AComponent: TComponent);
begin
  FDesignHelper.SelectObject(AComponent);
end;

procedure TdxGaugeControlDesignTimeSelectionHelper.ShowScalesEditor;
begin
  ShowFormEditorClass(FDesignHelper.Designer, Control as TdxCustomGaugeControl, TfmGaugeControlScalesEditor);
end;

procedure TdxGaugeControlDesignTimeSelectionHelper.DesignSelectionChanged(ASelection: TList);
begin
  Selections.Clear;
  PopulateSelections(ASelection);
  IsActive := GetActive(ASelection);
  Changed;
end;

function TdxGaugeControlDesignTimeSelectionHelper.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TdxGaugeControlDesignTimeSelectionHelper._AddRef: Integer;
begin
  Result := -1;
end;

function TdxGaugeControlDesignTimeSelectionHelper._Release: Integer;
begin
  Result := -1;
end;

procedure Register;

  procedure RegisterScaleClasses;
  var
    I: Integer;
    AScaleClasses: TList;
  begin
    AScaleClasses := TList.Create;
    try
      dxGaugeGetRegisteredScaleClasses(AScaleClasses);
      for I := 0 to AScaleClasses.Count - 1 do
      begin
        RegisterNoIcon([TdxGaugeCustomScaleClass(AScaleClasses[I])]);
        RegisterClasses([TdxGaugeCustomScaleClass(AScaleClasses[I])]);
      end;
    finally
      AScaleClasses.Free;
    end;
  end;

begin
  ForceDemandLoadState(dlDisable);
  RegisterComponents(dxCoreLibraryProductPage, [TdxGaugeControl]);
  RegisterNoIcon([
    TdxGaugeCircularScaleRange,
    TdxGaugeCircularWideScaleRange,
    TdxGaugeDigitalScaleCaption,
    TdxGaugeLinearScaleRange,
    TdxGaugeQuantitativeScaleCaption]);
  RegisterScaleClasses;
  RegisterComponentEditor(TdxGaugeControl, TdxGaugeControlComponentEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TdxGaugeCustomScale, 'AnchorScaleIndex', nil);
  RegisterPropertyEditor(TypeInfo(string), TdxGaugeCustomScale, 'StyleName', TdxGaugeStyleNameProperty);
  RegisterPropertyEditor(TypeInfo(TdxGaugeScaleCollection), TdxGaugeControl, 'Scales', TdxGaugeScaleCollectionProperty);
  RegisterPropertyEditor(TypeInfo(TdxGaugeCaptionCollection), TdxGaugeCustomScale, 'Captions', nil);
  RegisterPropertyEditor(TypeInfo(TdxGaugeRangeCollection), TdxGaugeQuantitativeScale, 'Ranges', nil);
  RegisterPropertyEditor(TypeInfo(Integer), TdxGaugeScaleOptionsArcLayout, 'Radius', nil);
  RegisterPropertyEditor(TypeInfo(Single), TdxGaugeScaleOptionsArcLayout, 'RadiusFactor', nil);
end;

initialization
  dxGaugeControlSelectionHelperClass := TdxGaugeControlDesignTimeSelectionHelper;

end.

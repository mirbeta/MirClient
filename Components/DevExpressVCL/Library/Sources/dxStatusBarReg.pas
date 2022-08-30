{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressStatusBar                                         }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSTATUSBAR AND ALL              }
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

unit dxStatusBarReg;

{$I cxVer.inc}

interface

procedure Register;

implementation

uses
  ColnEdit,
  DesignIntf, DesignEditors, VCLEditors, Types,
  Windows, SysUtils, Classes, Controls, Graphics, Forms, ImgList,
  cxClasses, cxPropEditors, dxBarReg, cxGraphics,
  dxStatusBar, dxStatusIndicatorEditor, cxLibraryReg;

type
  { TdxStatusBarEditor }

  TdxStatusBarEditor = class(TdxBarComponentEditor)
  protected
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
  end;

  { TdxStatusBarSelectionEditor }

  TdxStatusBarSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { TdxStatusBarPanelStyleProperty }

  TdxStatusBarPanelStyleProperty = class(TClassProperty)
  protected
    function CanAcceptPanelStyle(AClass: TClass): Boolean;
    function HasSubProperties: Boolean;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TdxStatusBarPanelStyleEventsProperty }

  TdxStatusBarPanelStyleEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  { TdxStatusBarStateIndicatorsProperty }

  TdxStatusBarStateIndicatorsProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TdxStatusBarTextPanelImageIndexProperty }

  TdxStatusBarTextPanelImageIndexProperty = class(TIntegerProperty, ICustomPropertyListDrawing)
  public
    function GetImages: TImageList; virtual;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer); virtual;
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer); virtual;
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); virtual;
  end;

{ TdxStatusBarEditor }

procedure TdxStatusBarEditor.InternalExecuteVerb(AIndex: Integer);
begin
  ShowCollectionEditor(Designer, Component, (Component as TdxStatusBar).Panels, 'Panels');
end;

function TdxStatusBarEditor.InternalGetVerb(AIndex: Integer): string;
begin
  Result := 'Panels Editor...';
end;

function TdxStatusBarEditor.InternalGetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TdxStatusBarSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('cxGraphics');
  dxSkinsRequiresAdditionalUnits(TdxCustomStatusBar, Proc);
end;

{ TdxStatusBarPanelStyleProperty }

function TdxStatusBarPanelStyleProperty.HasSubProperties: Boolean;
var
  I: Integer;
begin
  for I := 0 to PropCount - 1 do
  begin
    Result := TdxStatusBarPanel(GetComponent(I)).PanelStyle <> nil;
    if not Result then Exit;
  end;
  Result := True;
end;

function TdxStatusBarPanelStyleProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if not HasSubProperties then
    Exclude(Result, paSubProperties);
  Result := Result - [paReadOnly] +
    [paValueList, paSortList, paRevertable, paVolatileSubProperties];
end;

function TdxStatusBarPanelStyleProperty.GetValue: string;
begin
  if HasSubProperties then
    Result := GetRegisteredStatusBarPanelStyles.GetDescriptionByClass(TdxStatusBarPanelStyle(GetOrdValue).ClassType)
  else
    Result := '';
end;

procedure TdxStatusBarPanelStyleProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to GetRegisteredStatusBarPanelStyles.Count - 1 do
    if CanAcceptPanelStyle(GetRegisteredStatusBarPanelStyles[I]) then
      Proc(GetRegisteredStatusBarPanelStyles.Descriptions[I]);
end;

procedure TdxStatusBarPanelStyleProperty.SetValue(const Value: string);
var
  FPanelStyleStyleClass: TdxStatusBarPanelStyleClass;
  I: Integer;
begin
  FPanelStyleStyleClass := TdxStatusBarPanelStyleClass(
    GetRegisteredStatusBarPanelStyles.FindByClassName(Value));
  if FPanelStyleStyleClass = nil then
    FPanelStyleStyleClass := TdxStatusBarPanelStyleClass(
      GetRegisteredStatusBarPanelStyles.FindByDescription(Value));

  if CanAcceptPanelStyle(FPanelStyleStyleClass) then
  begin
    for I := 0 to PropCount - 1 do
      TdxStatusBarPanel(GetComponent(I)).PanelStyleClass := FPanelStyleStyleClass;
    Modified;
  end;
end;

function TdxStatusBarPanelStyleProperty.CanAcceptPanelStyle(AClass: TClass): Boolean;
begin
  Result := TdxStatusBarPanel(GetComponent(0)).StatusBarControl.CanAcceptPanelStyle(TdxStatusBarPanelStyleClass(AClass));
end;

{ TdxStatusBarPanelStyleEventsProperty }

function TdxStatusBarPanelStyleEventsProperty.GetInstance: TPersistent;
begin
  Result := TdxStatusBarPanel(GetComponent(0)).PanelStyle;
end;

{ TdxStatusBarStateIndicatorsProperty }

function TdxStatusBarStateIndicatorsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

procedure TdxStatusBarStateIndicatorsProperty.Edit;
var
  EditorForm: TdxStatusBarIndicatorEditor;
begin
  EditorForm := TdxStatusBarIndicatorEditor.Create(Application);
  try
    EditorForm.Caption :=
      TdxStatusBarStateIndicatorPanelStyle(GetComponent(0)).StatusBarControl.Name + '.IndicatorPanelStyle';
    EditorForm.Indicators.Assign(TdxStatusBarStateIndicatorPanelStyle(GetComponent(0)).Indicators);
    if EditorForm.ShowModal = mrOK then
    begin
      EditorForm.PrepareIndicators;
      with TdxStatusBarStateIndicatorPanelStyle(GetComponent(0)) do
      begin
        Indicators.BeginUpdate;
        try
          Indicators.Clear;
          Indicators.Assign(EditorForm.Indicators);
        finally
          Indicators.EndUpdate;
        end;
      end;
      Modified;
    end;
  finally
    FreeAndNil(EditorForm);
  end;
end;

{ TdxStatusBarTextPanelImageIndexProperty }

function TdxStatusBarTextPanelImageIndexProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paRevertable];
end;

function TdxStatusBarTextPanelImageIndexProperty.GetValue: string;
begin
  Result := IntToStr(GetOrdValue);
end;

procedure TdxStatusBarTextPanelImageIndexProperty.GetValues(Proc: TGetStrProc);
var i: Integer;
begin
  if GetImages <> nil then
    for i := 0 to GetImages.Count-1 do Proc(IntToStr(i));
end;

procedure TdxStatusBarTextPanelImageIndexProperty.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  AImageWidth: Integer;
begin
  with ACanvas do
  begin
    Brush.Style := bsSolid;
    if ASelected then Brush.Color := clHighlight else Brush.Color := clWindow;
    FillRect(ARect);
    if ASelected then DrawFocusRect(ARect);
    if GetImages <> nil then
      AImageWidth := GetImages.Width
    else
      AImageWidth := 0;
    if GetImages <> nil then
      GetImages.Draw(ACanvas, ARect.Left, ARect.Top, StrToInt(value));
    TextOut(ARect.Left + AImageWidth + 2, ARect.Top, Value);
  end;
end;

procedure TdxStatusBarTextPanelImageIndexProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
var
  AImageHeight, AStringHeight: Integer;
begin
  AStringHeight := ACanvas.TextHeight(Value);
  if GetImages <> nil then
    AImageHeight := GetImages.Height
  else
    AImageHeight := 0;

  if AStringHeight > AImageHeight then
    AHeight := AStringHeight + 2
  else
    AHeight := AImageHeight + 2;
end;

procedure TdxStatusBarTextPanelImageIndexProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
var
  AImageWidth, AStringWidth: Integer;
begin
  AStringWidth := ACanvas.TextWidth(Value);
  if GetImages <> nil then
    AImageWidth  := GetImages.Width
  else
    AImageWidth := 0;
  AWidth := AStringWidth + AImageWidth + 4;
end;

procedure TdxStatusBarTextPanelImageIndexProperty.SetValue(const Value: string);
begin
  SetOrdValue(StrToInt(Value));
end;

function TdxStatusBarTextPanelImageIndexProperty.GetImages: TImageList;
begin
  Result := nil;
  if GetComponent(0) is TdxStatusBarTextPanelStyle then
    Result := TImageList(TdxStatusBarTextPanelStyle(GetComponent(0)).StatusBarControl.Images);
end;

procedure Register;
begin
  RegisterComponents(dxBarProductPage, [TdxStatusBar]);
  RegisterComponentEditor(TdxStatusBar, TdxStatusBarEditor);
  RegisterPropertyEditor(TypeInfo(TdxStatusBarPanelStyle), TdxStatusBarPanel, 'PanelStyle', TdxStatusBarPanelStyleProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TdxStatusBarPanel, 'PanelStyleEvents', TdxStatusBarPanelStyleEventsProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxStatusBarPanel, 'PanelStyleClassName', nil);
  RegisterPropertyEditor(TypeInfo(TdxStatusBarContainerControl), TdxStatusBarContainerPanelStyle, 'Container', nil);
  RegisterPropertyEditor(TypeInfo(TdxStatusBarStateIndicators), TdxStatusBarPanelStyle, '',
    TdxStatusBarStateIndicatorsProperty);
  RegisterPropertyEditor(TypeInfo(TcxImageIndex), TdxStatusBarTextPanelStyle, 'ImageIndex', TdxStatusBarTextPanelImageIndexProperty);
  RegisterSelectionEditor(TdxStatusBar, TdxStatusBarSelectionEditor);
end;

end.

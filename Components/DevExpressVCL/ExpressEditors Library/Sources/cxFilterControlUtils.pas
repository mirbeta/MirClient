{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressFilterControl                                     }
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

unit cxFilterControlUtils;

{$I cxVer.inc}

interface

uses
  Variants, Classes, Controls, Forms, SysUtils, dxCore, cxClasses, cxDataStorage,
  cxEdit, cxFilter, cxLookAndFeels;

type
  TcxFilterControlOperator = (fcoNone, fcoEqual, fcoNotEqual, fcoLess,
    fcoLessEqual, fcoGreater, fcoGreaterEqual, fcoLike, fcoNotLike,
    fcoContains, fcoNotContains, fcoBeginsWith, fcoEndsWith,
    fcoBlanks, fcoNonBlanks, fcoBetween, fcoNotBetween, fcoInList,
    fcoNotInList, fcoYesterday, fcoToday, fcoTomorrow,
    fcoLast7Days, fcoLastWeek, fcoLast14Days, fcoLastTwoWeeks, fcoLast30Days, fcoLastMonth, fcoLastYear, fcoInPast,
    fcoThisWeek, fcoThisMonth, fcoThisYear,
    fcoNext7Days, fcoNextWeek, fcoNext14Days, fcoNextTwoWeeks, fcoNext30Days, fcoNextMonth, fcoNextYear, fcoInFuture);

  TcxFilterControlOperators = set of TcxFilterControlOperator;

  EcxFilterControlError = class(EdxException);

  TcxFilterDataType = (fdtText, fdtNumeric, fdtDate, fdtTime, fdtLookup, fdtCheck);

  { TcxCustomFilterEditHelper }

  TcxCustomFilterEditHelper = class
  protected
    class procedure ClearPropertiesEvents(AProperties: TcxCustomEditProperties); virtual;
    class procedure InitializeEdit(AEdit: TcxCustomEdit;
      AEditProperties: TcxCustomEditProperties); virtual;
    class function IsIDefaultValuesProviderNeeded(
      AEditProperties: TcxCustomEditProperties): Boolean; virtual;
  public
    class function EditPropertiesHasButtons: Boolean; virtual;
    class function GetFilterDataType(AValueTypeClass: TcxValueTypeClass): TcxFilterDataType; virtual;
    class function GetFilterEdit(AEditProperties: TcxCustomEditProperties;
      AInplaceEditList: TcxInplaceEditList = nil): TcxCustomEdit;
    class function GetFilterEditClass: TcxCustomEditClass; virtual;
    class procedure GetFilterValue(AEdit: TcxCustomEdit;
      AEditProperties: TcxCustomEditProperties; var V: Variant; var S: TCaption); virtual;
    class function GetSupportedFilterOperators(
      AProperties: TcxCustomEditProperties;
      AValueTypeClass: TcxValueTypeClass;
      AExtendedSet: Boolean = False): TcxFilterControlOperators; virtual;
    class procedure InitializeProperties(AProperties,
      AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean); virtual;
    class function IsValueValid(AValueTypeClass: TcxValueTypeClass;
      var AValue: Variant): Boolean; virtual;
    class procedure SetFilterValue(AEdit: TcxCustomEdit;
      AEditProperties: TcxCustomEditProperties; AValue: Variant); virtual;
    class function UseDisplayValue: Boolean; virtual;
  end;

  TcxCustomFilterEditHelperClass = class of TcxCustomFilterEditHelper;

  { TcxFilterEditsController }

  TcxFilterEditsController = class(TcxRegisteredClassList)
  public
    function FindHelper(APropertiesClass: TClass): TcxCustomFilterEditHelperClass;
    procedure Unregister(AItemClass, ARegisteredClass: TClass); override;
  end;

function GetFilterOperatorKind(AOperator: TcxFilterControlOperator): TcxFilterOperatorKind;
function GetFilterControlOperator(AOperatorKind: TcxFilterOperatorKind; AIsNull: Boolean): TcxFilterControlOperator;

procedure CorrectDlgParams(var Params: TCreateParams);
procedure FilterControlError(const Msg: string);
procedure FilterControlValidateValue(AEdit: TcxCustomEdit; var AValue: Variant;
  AOperator: TcxFilterControlOperator; AValueTypeClass: TcxValueTypeClass;
  AFilterEditHelper: TcxCustomFilterEditHelperClass);
function FilterEditsController: TcxFilterEditsController;
function GetFilterControlOperatorText(AOperator: TcxFilterControlOperator): string;

implementation

uses
  cxFilterConsts, cxFilterControlStrs;

var
  FController: TcxFilterEditsController;

function GetFilterOperatorKind(AOperator: TcxFilterControlOperator): TcxFilterOperatorKind;
const
  OperatorMap: array[TcxFilterControlOperator] of TcxFilterOperatorKind = (
    foEqual, foEqual, foNotEqual, foLess, foLessEqual,
    foGreater, foGreaterEqual, foLike, foNotLike, foContains, foNotContains, foBeginsWith, foEndsWith,
    foEqual, foNotEqual, // blank - non blank
    foBetween, foNotBetween, foInList, foNotInList, foYesterday, foToday, foTomorrow,
    foLast7Days, foLastWeek, foLast14Days, foLastTwoWeeks, foLast30Days, foLastMonth, foLastYear, foInPast,
    foThisWeek, foThisMonth, foThisYear,
    foNext7Days, foNextWeek, foNext14Days, foNextTwoWeeks, foNext30Days, foNextMonth, foNextYear, foInFuture);
begin
  Result := OperatorMap[AOperator];
end;

function GetFilterControlOperator(AOperatorKind: TcxFilterOperatorKind; AIsNull: Boolean): TcxFilterControlOperator;
const
  OperatorKindMap: array[TcxFilterOperatorKind] of TcxFilterControlOperator = (
    fcoEqual, fcoNotEqual, fcoLess, fcoLessEqual, fcoGreater, fcoGreaterEqual,
    fcoLike, fcoNotLike, fcoBetween, fcoNotBetween, fcoInList, fcoNotInList,
    fcoYesterday, fcoToday, fcoTomorrow,
    fcoLast7Days, fcoLastWeek, fcoLast14Days, fcoLastTwoWeeks, fcoLast30Days, fcoLastMonth, fcoLastYear, fcoInPast,
    fcoThisWeek, fcoThisMonth, fcoThisYear,
    fcoNext7Days, fcoNextWeek, fcoNext14Days, fcoNextTwoWeeks, fcoNext30Days, fcoNextMonth, fcoNextYear, fcoInFuture,
    fcoContains, fcoNotContains, fcoBeginsWith, fcoEndsWith);
  ExtOperatorKindMap: array[Boolean] of TcxFilterControlOperator =
    (fcoNonBlanks, fcoBlanks);
begin
  Result := OperatorKindMap[AOperatorKind];
  if (Result in [fcoEqual, fcoNotEqual]) and AIsNull then
    Result := ExtOperatorKindMap[Result = fcoEqual];
end;

procedure CorrectDlgParams(var Params: TCreateParams);
var
  I: Integer;
  AActiveForm: TForm;
begin
  AActiveForm := Screen.ActiveForm;
  if AActiveForm <> nil then
  begin
    for I := 0 to Screen.FormCount - 1 do
      if (Screen.Forms[I] <> AActiveForm) and (Screen.Forms[I].FormStyle = fsStayOnTop) then
      begin
        AActiveForm := nil;
        break;
      end;
  end;
  if AActiveForm <> nil then
    Params.WndParent := AActiveForm.Handle;
end;

procedure FilterControlError(const Msg: string);
begin
  raise EcxFilterControlError.Create(Msg);
end;

procedure FilterControlValidateValue(AEdit: TcxCustomEdit; var AValue: Variant;
  AOperator: TcxFilterControlOperator; AValueTypeClass: TcxValueTypeClass;
  AFilterEditHelper: TcxCustomFilterEditHelperClass);
var
  AError: Boolean;
begin
  if AValueTypeClass = nil then
    Exit;
  if VarIsStr(AValue) and (AValue = '') and not AValueTypeClass.IsString then
    AValue := Null;
  if VarIsNull(AValue) or (AOperator in [fcoLike, fcoNotLike,
    fcoContains, fcoNotContains, fcoBeginsWith, fcoEndsWith]) then
    Exit;
  AError := True;
  try
    AError := not AFilterEditHelper.IsValueValid(AValueTypeClass, AValue);
  finally
    if AError then
    begin
      FilterControlError(cxGetResourceString(@cxSFilterDialogInvalidValue));
      AEdit.EditModified := True;
      if (AEdit <> nil) and AEdit.CanFocusEx then
        AEdit.SetFocus;
    end;
  end;
end;

function FilterEditsController: TcxFilterEditsController;
begin
  if FController = nil then
    FController := TcxFilterEditsController.Create;
  Result := FController;
end;

function GetFilterControlOperatorText(AOperator: TcxFilterControlOperator): string;
begin
  case AOperator of
    fcoEqual:
      Result := cxGetResourceString(@cxSFilterOperatorEqual);
    fcoNotEqual:
      Result := cxGetResourceString(@cxSFilterOperatorNotEqual);
    fcoLess:
      Result := cxGetResourceString(@cxSFilterOperatorLess);
    fcoLessEqual:
      Result := cxGetResourceString(@cxSFilterOperatorLessEqual);
    fcoGreater:
      Result := cxGetResourceString(@cxSFilterOperatorGreater);
    fcoGreaterEqual:
      Result := cxGetResourceString(@cxSFilterOperatorGreaterEqual);
    fcoLike:
      Result := cxGetResourceString(@cxSFilterOperatorLike);
    fcoNotLike:
      Result := cxGetResourceString(@cxSFilterOperatorNotLike);
    fcoContains:
      Result := cxGetResourceString(@cxSFilterOperatorContains);
    fcoNotContains:
      Result := cxGetResourceString(@cxSFilterOperatorDoesNotContain);
    fcoBeginsWith:
      Result := cxGetResourceString(@cxSFilterOperatorBeginsWith);
    fcoEndsWith:
      Result := cxGetResourceString(@cxSFilterOperatorEndsWith);
    fcoBlanks:
      Result := cxGetResourceString(@cxSFilterOperatorIsNull);
    fcoNonBlanks:
      Result := cxGetResourceString(@cxSFilterOperatorIsNotNull);
    fcoBetween:
      Result := cxGetResourceString(@cxSFilterOperatorBetween);
    fcoNotBetween:
      Result := cxGetResourceString(@cxSFilterOperatorNotBetween);
    fcoInList:
      Result := cxGetResourceString(@cxSFilterOperatorInList);
    fcoNotInList:
      Result := cxGetResourceString(@cxSFilterOperatorNotInList);
    //date
    fcoYesterday:
      Result := cxGetResourceString(@cxSFilterOperatorYesterday);
    fcoToday:
      Result := cxGetResourceString(@cxSFilterOperatorToday);
    fcoTomorrow:
      Result := cxGetResourceString(@cxSFilterOperatorTomorrow);
    fcoLast7Days:
      Result := cxGetResourceString(@cxSFilterOperatorLast7Days);
    fcoLastWeek:
      Result := cxGetResourceString(@cxSFilterOperatorLastWeek);
    fcoLast14Days:
      Result := cxGetResourceString(@cxSFilterOperatorLast14Days);
    fcoLastTwoWeeks:
      Result := cxGetResourceString(@cxSFilterOperatorLastTwoWeeks);
    fcoLast30Days:
      Result := cxGetResourceString(@cxSFilterOperatorLast30Days);
    fcoLastMonth:
      Result := cxGetResourceString(@cxSFilterOperatorLastMonth);
    fcoLastYear:
      Result := cxGetResourceString(@cxSFilterOperatorLastYear);
    fcoInPast:
      Result := cxGetResourceString(@cxSFilterOperatorPast);
    fcoThisWeek:
      Result := cxGetResourceString(@cxSFilterOperatorThisWeek);
    fcoThisMonth:
      Result := cxGetResourceString(@cxSFilterOperatorThisMonth);
    fcoThisYear:
      Result := cxGetResourceString(@cxSFilterOperatorThisYear);
    fcoNext7Days:
      Result := cxGetResourceString(@cxSFilterOperatorNext7Days);
    fcoNextWeek:
      Result := cxGetResourceString(@cxSFilterOperatorNextWeek);
    fcoNext14Days:
      Result := cxGetResourceString(@cxSFilterOperatorNext14Days);
    fcoNextTwoWeeks:
      Result := cxGetResourceString(@cxSFilterOperatorNextTwoWeeks);
    fcoNext30Days:
      Result := cxGetResourceString(@cxSFilterOperatorNext30Days);
    fcoNextMonth:
      Result := cxGetResourceString(@cxSFilterOperatorNextMonth);
    fcoNextYear:
      Result := cxGetResourceString(@cxSFilterOperatorNextYear);
    fcoInFuture:
      Result := cxGetResourceString(@cxSFilterOperatorFuture);
    else
      Result := '';
  end;
end;

{ TcxCustomFilterEditHelper }

class procedure TcxCustomFilterEditHelper.ClearPropertiesEvents(
  AProperties: TcxCustomEditProperties);
begin
  if AProperties = nil then Exit;
  with AProperties do
  begin
    OnValidate := nil;
    OnEditValueChanged := nil;
    OnChange := nil;
    OnButtonClick := nil;
  end;
end;

class procedure TcxCustomFilterEditHelper.InitializeEdit(AEdit: TcxCustomEdit;
  AEditProperties: TcxCustomEditProperties);
begin
  InitializeProperties(AEdit.ActiveProperties, AEditProperties,
    EditPropertiesHasButtons);
end;

class function TcxCustomFilterEditHelper.IsIDefaultValuesProviderNeeded(
  AEditProperties: TcxCustomEditProperties): Boolean;
begin
  Result := True;
end;

class function TcxCustomFilterEditHelper.EditPropertiesHasButtons: Boolean;
begin
  Result := False;
end;

class function TcxCustomFilterEditHelper.GetFilterDataType(AValueTypeClass: TcxValueTypeClass): TcxFilterDataType;
begin
  Result := fdtText;
  if AValueTypeClass = nil then
    Exit;
  if IsDateTimeValueTypeClass(AValueTypeClass) then
    Result := fdtDate
  else
    if not AValueTypeClass.IsString then
      Result := fdtNumeric;
end;

class function TcxCustomFilterEditHelper.GetFilterEdit(AEditProperties: TcxCustomEditProperties;
  AInplaceEditList: TcxInplaceEditList = nil): TcxCustomEdit;
begin
  if AInplaceEditList = nil then
    Result := GetFilterEditClass.Create(nil)
  else
    Result := AInplaceEditList.GetEdit(TcxCustomEditPropertiesClass(GetFilterEditClass.GetPropertiesClass));
  InitializeEdit(Result, AEditProperties);
end;

class procedure TcxCustomFilterEditHelper.GetFilterValue(AEdit: TcxCustomEdit;
  AEditProperties: TcxCustomEditProperties; var V: Variant; var S: TCaption);
begin
  V := AEdit.EditValue;
  S := AEditProperties.GetDisplayText(V);
  if S = '' then
    S := VarToStr(V);
end;

class function TcxCustomFilterEditHelper.GetSupportedFilterOperators(
  AProperties: TcxCustomEditProperties;
  AValueTypeClass: TcxValueTypeClass;
  AExtendedSet: Boolean = False): TcxFilterControlOperators;
begin
  if AExtendedSet then
    Result := [fcoEqual..fcoNotInList]
  else
    Result := [fcoEqual..fcoNonBlanks];
  if (AValueTypeClass = nil) or not AValueTypeClass.IsString then
    Result := Result - [fcoLike, fcoNotLike, fcoContains, fcoNotContains, fcoBeginsWith, fcoEndsWith];
end;

class function TcxCustomFilterEditHelper.GetFilterEditClass: TcxCustomEditClass;
begin
  Result := nil;
end;

class procedure TcxCustomFilterEditHelper.InitializeProperties(
  AProperties, AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean);
begin
  AProperties.Assign(AEditProperties);
  AProperties.Alignment.Horz := taLeftJustify;
  AProperties.Alignment.Vert := taVCenter;
  AProperties.BeepOnError := False;
  if not AHasButtons then
    AProperties.Buttons.Clear;
  AProperties.ReadOnly := False;
  AProperties.ValidateOnEnter := True;
  if not IsIDefaultValuesProviderNeeded(AEditProperties) then
    AProperties.IDefaultValuesProvider := nil;
  ClearPropertiesEvents(AProperties);
end;

class function TcxCustomFilterEditHelper.IsValueValid(AValueTypeClass: TcxValueTypeClass;
  var AValue: Variant): Boolean;
begin
  Result := AValueTypeClass.IsValueValid(AValue);
end;

class procedure TcxCustomFilterEditHelper.SetFilterValue(AEdit: TcxCustomEdit;
  AEditProperties: TcxCustomEditProperties; AValue: Variant);
begin
end;

class function TcxCustomFilterEditHelper.UseDisplayValue: Boolean;
begin
  Result := False;
end;

{ TcxFilterEditsController }

function TcxFilterEditsController.FindHelper(APropertiesClass: TClass): TcxCustomFilterEditHelperClass;
begin
  if APropertiesClass <> nil then
  begin
    Result := TcxCustomFilterEditHelperClass(FindClass(APropertiesClass));
    if (Result = nil) and (APropertiesClass <> nil) then
      Result := FindHelper(APropertiesClass.ClassParent);
  end
  else
    Result := nil;
end;

procedure TcxFilterEditsController.Unregister(AItemClass,
  ARegisteredClass: TClass);
begin
  inherited Unregister(AItemClass, ARegisteredClass);
  if Count = 0 then
    FreeAndNil(FController);
end;

end.

{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars components                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSBARS AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
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

unit cxBarEditItemValueEditor;

{$I cxVer.inc}

interface

uses
  dxCore, dxBar, cxBarEditItem, dxForms;

function ShowValueEditor(AItem: TcxCustomBarEditItem): Boolean; overload;
function ShowValueEditor(AItemLink: TdxBarItemLink): Boolean; overload;

implementation

uses
  Windows, Classes, Controls, ExtCtrls, Forms, Math, StdCtrls, SysUtils, TypInfo,
  cxButtons, cxClasses, cxEdit, cxFilterControlUtils, cxLookAndFeelPainters,
  dxBarStrs;

type
  TControlAccess = class(TControl);
  TcxCustomEditAccess = class(TcxCustomEdit);
  TdxBarManagerAccess = class(TdxBarManager);

procedure ClearPublishedEvents(AInstance: TObject);
var
  ACount, I: Integer;
  AMethod: TMethod;
  APPropList: PPropList;
begin
  ACount := GetPropList(PTypeInfo(AInstance.ClassInfo), [tkMethod], nil);
  if ACount = 0 then
    Exit;
  GetMem(APPropList, ACount * SizeOf(Pointer));
  try
    GetPropList(PTypeInfo(AInstance.ClassInfo), [tkMethod], APPropList);
    for I := 0 to ACount - 1 do
    begin
      AMethod := GetMethodProp(AInstance, APPropList[I]);
      if AMethod.Code <> nil then
      begin
        AMethod.Code := nil;
        AMethod.Data := nil;
        SetMethodProp(AInstance, APPropList[I], AMethod);
      end;
    end;
  finally
    FreeMem(APPropList);
  end;
end;

function InternalShowValueEditor(AItem: TcxCustomBarEditItem;
  AItemLink: TdxBarItemLink = nil): Boolean;
var
  ABevel: TBevel;
  AButtonOK, AButtonCancel: TcxButton;
  AEdit: TcxCustomEdit;
  AForm: TForm;

  function UseFilterHelper: Boolean;
  begin
    Result := not (esoEditing in AItem.GetProperties.GetSupportedOperations);
  end;

  procedure CreateEdit;
  var
    AFilterEditHelper: TcxCustomFilterEditHelperClass;
    AProperties: TcxCustomEditProperties;
  begin
    AProperties := AItem.GetProperties;
    if UseFilterHelper then
    begin
      AFilterEditHelper := FilterEditsController.FindHelper(AProperties.ClassType);
      if AFilterEditHelper = nil then
        raise EdxException.Create('');
      AEdit := AFilterEditHelper.GetFilterEdit(AProperties);
      AEdit.Parent := AForm;
      AFilterEditHelper.SetFilterValue(AEdit, AProperties, AItem.EditValue);
    end
    else
    begin
      AEdit := TcxCustomEditClass(AProperties.GetContainerClass).Create(nil);
      AEdit.LockChangeEvents(True);
      try
        AEdit.InternalProperties.Assign(AProperties);
      finally
        AEdit.LockChangeEvents(False, False);
      end;
      AEdit.Parent := AForm;
      AEdit.EditValue := AItem.EditValue;
      if esoShowingCaption in AProperties.GetSupportedOperations then
        if AItemLink <> nil then
          TControlAccess(AEdit).Caption := AItemLink.Caption
        else
          TControlAccess(AEdit).Caption := AItem.Caption;
    end;
    AEdit.Style.HotTrack := False;
    TdxBarManagerAccess(AItem.BarManager).GetRealLookAndFeel(AEdit.Style.LookAndFeel);
  end;

  function GetEditSize(AEdit: TcxCustomEdit): TSize;
  begin
    if AItemLink <> nil then
      Result.cx := AItemLink.Width
    else
      Result.cx := 121;

    if esfMinSize in AEdit.ActiveProperties.GetSpecialFeatures then
      TcxCustomEditAccess(AEdit).AutoSize := True;
    AEdit.HandleNeeded;
    Result.cy := AEdit.Height;
  end;

  function CreateButton(AParent: TForm; const ACaption: string;
    AModalResult: TModalResult): TcxButton;
  begin
    Result := TcxButton.Create(AParent);
    Result.Caption := ACaption;
    Result.ModalResult := AModalResult;
    Result.Parent := AParent;
    TdxBarManagerAccess(AItem.BarManager).GetRealLookAndFeel(Result.LookAndFeel);
  end;

  procedure CreateSubControls;
  begin
    ABevel := TBevel.Create(AForm);
    ABevel.Parent := AForm;

    AButtonOK := CreateButton(AForm, cxGetResourceString(@dxSBAR_DIALOGOK), mrOk);
    AButtonOK.Default := True;

    AButtonCancel := CreateButton(AForm, cxGetResourceString(@dxSBAR_DIALOGCANCEL), mrCancel);
    AButtonCancel.Cancel := True;
  end;

  procedure PrepareEditForClose;
  begin
    AItem.GetProperties.BeginUpdate;
    try
      AEdit.ActiveProperties.Update(AItem.GetProperties);
    finally
      AItem.GetProperties.EndUpdate(False);
    end;
  end;

const
  ABevelHeight = 2;
var
  AEditSize: TSize;
  AProperties: TcxCustomEditProperties;
  D, H, W: Integer;
  S: TCaption;
  V: Variant;
begin
  AForm := TdxForm.CreateNew(nil);
  try
    AForm.BiDiMode := AItem.BarManager.BiDiMode;
    AForm.BorderStyle := bsDialog;
    AForm.Caption := cxGetResourceString(@dxSBAR_CXEDITVALUEDIALOGCAPTION);
    if AItemLink <> nil then
      AForm.Font := AItemLink.BarControl.Font
    else
      AForm.Font := AItem.BarManager.Font;

    CreateEdit;
    CreateSubControls;

    AEditSize := GetEditSize(AEdit);
    AForm.Canvas.Font := AForm.Font;
    W := 12 * AForm.Canvas.TextWidth('0');
    H := MulDiv(AForm.Canvas.TextHeight('0'), 5, 3);
    D := H div 4;

    AForm.ClientWidth := D + Max(AEditSize.cx, W + D * 2 + W) + D;
    AForm.ClientHeight := D + AEditSize.cy + D + ABevelHeight + D + H + D;
    AForm.Position := poScreenCenter;

    AEdit.SetBounds(D, D, AForm.ClientWidth - D * 2, AEditSize.cy);
    ABevel.SetBounds(D, D + AEditSize.cy + D, AForm.ClientWidth - D * 2, 2);
    AButtonOK.SetBounds((AForm.ClientWidth - (W + D * 2 + W)) div 2,
      D + AEditSize.cy + D + ABevelHeight + D, W, H);
    AButtonCancel.SetBounds(AButtonOK.BoundsRect.Right + D * 2, AButtonOK.Top, W, H);

    Result := AForm.ShowModal = mrOk;
    if Result then
    begin
      AProperties := AItem.GetProperties;
      if UseFilterHelper then
      begin
        FilterEditsController.FindHelper(AProperties.ClassType).GetFilterValue(AEdit, AProperties, V, S);
        AItem.EditValue := V;
      end
      else
      begin
        PrepareEditForClose;
        AItem.EditValue := AEdit.EditValue;
      end;
    end;
  finally
    FreeAndNil(AForm);
  end;
end;

function ShowValueEditor(AItem: TcxCustomBarEditItem): Boolean;
begin
  Result := InternalShowValueEditor(AItem);
end;

function ShowValueEditor(AItemLink: TdxBarItemLink): Boolean;
begin
  Result := InternalShowValueEditor(TcxCustomBarEditItem(AItemLink.Item), AItemLink);
end;

end.

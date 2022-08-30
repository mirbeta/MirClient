{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressScheduler                                         }
{                                                                    }
{           Copyright (c) 2003-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSCHEDULER AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit cxSchedulerEditorFormManager;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Controls, cxClasses, Graphics, cxSchedulerStorage, dxCore, dxCoreClasses,
  Forms;

type

  { TcxShedulerEditorFormStyleInfo }

  TcxShedulerEventEditorFormStyleInfo = class(TPersistent)
  public
    class function CreateEditor(AEvent: TcxSchedulerControlEvent): IcxSchedulerEventEditorForm; virtual;
    class function GetName: string; virtual;
    class function GetUnitName: string; virtual;
  end;

  TcxShedulerEventEditorFormStyleInfoClass = class of TcxShedulerEventEditorFormStyleInfo;

  { TcxSchedulerEventEditorFormManager }

  TcxSchedulerEventEditorFormManager = class
  private
    FList: TdxFastList;
    FItemIndex: Integer;

    procedure CheckItemIndex;
    function GetCount: Integer;
    function GetCurrentEditorFormStyle: TCaption;
    function GetCurrentEditorFormStyleInfo: TcxShedulerEventEditorFormStyleInfoClass;
    function GetItem(Index: Integer): TcxShedulerEventEditorFormStyleInfoClass; inline;
    function IsItemIndexValid: Boolean;
    function GetName(Index: Integer): string;
    procedure SetCurrentEditorFormStyle(AValue: TCaption);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function CreateEditor(const ADialogsStyle: string; AEvent: TcxSchedulerControlEvent): IcxSchedulerEventEditorForm;
    function GetIndexByName(AValue: string): Integer;
    procedure PopulateShedulerEditorFormList(AStrings: TStrings);
    procedure RegisterShedulerEditorForm(AShedulerEditorFormStyleInfo: TcxShedulerEventEditorFormStyleInfoClass);
    procedure UnregisterShedulerEditorForm(AShedulerEditorFormStyleInfo: TcxShedulerEventEditorFormStyleInfoClass);

    property Count: Integer read GetCount;
    property CurrentEditorFormStyle: TCaption read GetCurrentEditorFormStyle write SetCurrentEditorFormStyle;
    property CurrentEditorFormStyleInfo: TcxShedulerEventEditorFormStyleInfoClass read GetCurrentEditorFormStyleInfo;
    property Items[Index: Integer]: TcxShedulerEventEditorFormStyleInfoClass read GetItem;
    property ItemIndex: Integer read FItemIndex write FItemIndex;
    property Names[Index: Integer]: string read GetName;
  end;

function cxSchedulerEditorManager: TcxSchedulerEventEditorFormManager;

implementation

uses
  RTLConsts;

var
  FSchedulerEditorFormManager: TcxSchedulerEventEditorFormManager;

function cxSchedulerEditorManager: TcxSchedulerEventEditorFormManager;
begin
  if FSchedulerEditorFormManager = nil then
    FSchedulerEditorFormManager := TcxSchedulerEventEditorFormManager.Create;
  Result := FSchedulerEditorFormManager;
end;

{ TcxShedulerEditorFormStyleInfo }

class function TcxShedulerEventEditorFormStyleInfo.CreateEditor(AEvent: TcxSchedulerControlEvent): IcxSchedulerEventEditorForm;
begin
  Result := nil;
end;

class function TcxShedulerEventEditorFormStyleInfo.GetName: string;
begin
  Result := '';
end;

class function TcxShedulerEventEditorFormStyleInfo.GetUnitName: string;
begin
  Result := cxGetUnitName(Self);
end;

{ TcxSchedulerEventEditorFormManager }

constructor TcxSchedulerEventEditorFormManager.Create;
begin
  inherited Create;
  FList := TdxFastList.Create;
  FItemIndex := -1;
end;

destructor TcxSchedulerEventEditorFormManager.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TcxSchedulerEventEditorFormManager.CreateEditor(const ADialogsStyle: string;
  AEvent: TcxSchedulerControlEvent): IcxSchedulerEventEditorForm;
var
  AIndex: Integer;
begin
  AIndex := GetIndexByName(ADialogsStyle);
  if AIndex <> -1 then
    Result := Items[AIndex].CreateEditor(AEvent)
  else
    Result := CurrentEditorFormStyleInfo.CreateEditor(AEvent);
end;

function TcxSchedulerEventEditorFormManager.GetIndexByName(AValue: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if CompareText(Items[I].GetName, AValue) = 0 then
      Result := I;
end;

procedure TcxSchedulerEventEditorFormManager.PopulateShedulerEditorFormList(AStrings: TStrings);
var
  I: Integer;
begin
  AStrings.BeginUpdate;
  try
    for I := 0 to Count - 1 do
      AStrings.AddObject(Names[I], TObject(Items[I]));
  finally
    AStrings.EndUpdate;
  end;
end;

procedure TcxSchedulerEventEditorFormManager.RegisterShedulerEditorForm(
  AShedulerEditorFormStyleInfo: TcxShedulerEventEditorFormStyleInfoClass);
begin
  FList.Add(AShedulerEditorFormStyleInfo);
end;

procedure TcxSchedulerEventEditorFormManager.UnregisterShedulerEditorForm(
  AShedulerEditorFormStyleInfo: TcxShedulerEventEditorFormStyleInfoClass);
begin
  FList.Remove(AShedulerEditorFormStyleInfo);
  CheckItemIndex;
end;

function TcxSchedulerEventEditorFormManager.GetCurrentEditorFormStyleInfo: TcxShedulerEventEditorFormStyleInfoClass;
begin
  CheckItemIndex;
  Result := TcxShedulerEventEditorFormStyleInfoClass(FList[ItemIndex])
end;

function TcxSchedulerEventEditorFormManager.GetCurrentEditorFormStyle: TCaption;
begin
  Result := CurrentEditorFormStyleInfo.GetName;
end;

procedure TcxSchedulerEventEditorFormManager.CheckItemIndex;
begin
  if not IsItemIndexValid then
    ItemIndex := 0;
end;

function TcxSchedulerEventEditorFormManager.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TcxSchedulerEventEditorFormManager.GetItem(Index: Integer): TcxShedulerEventEditorFormStyleInfoClass;
begin
  Result := TcxShedulerEventEditorFormStyleInfoClass(FList.Items[Index]);
end;

function TcxSchedulerEventEditorFormManager.IsItemIndexValid: Boolean;
begin
  Result := (FItemIndex >= 0) and (FItemIndex < Count);
end;

function TcxSchedulerEventEditorFormManager.GetName(Index: Integer): string;
begin
  Result := Items[Index].GetName;
end;

procedure TcxSchedulerEventEditorFormManager.SetCurrentEditorFormStyle(AValue: TCaption);
begin
  ItemIndex := GetIndexByName(AValue);
end;

procedure Initialize;
begin
end;

procedure Finalize;
begin
  FreeAndNil(FSchedulerEditorFormManager);
end;

initialization
  dxUnitsLoader.AddUnit(@Initialize, @Finalize);

finalization
  dxUnitsLoader.RemoveUnit(@Finalize);

end.

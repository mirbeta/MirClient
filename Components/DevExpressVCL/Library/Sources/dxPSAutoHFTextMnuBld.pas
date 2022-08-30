{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSAutoHFTextMnuBld;

interface

{$I cxVer.inc}

uses
  Classes, dxCore, cxClasses, dxBase, dxPSSngltn, dxPgsDlg;

type
  TAbstractdxPSAutoHFTextMenuBuilder = class
  public
    constructor Create; virtual;
    procedure BuildAutoHFTextEntriesMenu(ARootItem: TObject; AData: Pointer;
      AIncludeSetupAutoHFTextEntriesItem: Boolean; AAutoHFTextEntries: TStrings;
      AOnHFTextEntriesClick, AOnSetupHFTextEntriesClick: TNotifyEvent); virtual; abstract;
    class function ExtractAutoHFTextEntryIndexFromObj(Obj: TObject): Integer; virtual;
  end;

  TdxPSAutoHFTextMenuBuilderClass = class of TAbstractdxPSAutoHFTextMenuBuilder;

  TdxStandardPSAutoHFTextMenuBuilder = class(TAbstractdxPSAutoHFTextMenuBuilder)
  public
    procedure BuildAutoHFTextEntriesMenu(ARootItem: TObject; AData: Pointer;
      AIncludeSetupAutoHFTextEntriesItem: Boolean; AAutoHFTextEntries: TStrings;
      AOnHFTextEntriesClick, AOnSetupHFTextEntriesClick: TNotifyEvent); override;
    class function ExtractAutoHFTextEntryIndexFromObj(Obj: TObject): Integer; override;
  end;

  TdxPSAutoHFTextMenuBuilderFactory = class(TBasedxPSSingleton)
  private
    FBuilders: TdxClassList;
    function GetActiveBuilder: TdxPSAutoHFTextMenuBuilderClass;
    function GetBuilder(Index: Integer): TdxPSAutoHFTextMenuBuilderClass;
    function GetCount: Integer;
  protected
    procedure FinalizeInstance; override;
    procedure InitializeInstance; override;
  public
    class function Instance: TdxPSAutoHFTextMenuBuilderFactory; reintroduce; overload;
    procedure RegisterBuilder(ABuilder: TdxPSAutoHFTextMenuBuilderClass);
    procedure UnregisterBuilder(ABuilder: TdxPSAutoHFTextMenuBuilderClass);

    property ActiveBuilder: TdxPSAutoHFTextMenuBuilderClass read GetActiveBuilder;
    property Builders[Index: Integer]: TdxPSAutoHFTextMenuBuilderClass read GetBuilder;
    property Count: Integer read GetCount;
  end;

function dxPSAutoHFTextMenuBuilderFactory: TdxPSAutoHFTextMenuBuilderFactory;

implementation

uses
  Menus, SysUtils, dxPSUtl, dxPSRes;

function dxPSAutoHFTextMenuBuilderFactory: TdxPSAutoHFTextMenuBuilderFactory;
begin
  Result := TdxPSAutoHFTextMenuBuilderFactory.Instance;
end;

{ TdxPSAutoHFTextMenuBuilderFactory }

class function TdxPSAutoHFTextMenuBuilderFactory.Instance: TdxPSAutoHFTextMenuBuilderFactory;
begin
  Result := inherited Instance as TdxPSAutoHFTextMenuBuilderFactory;
end;

procedure TdxPSAutoHFTextMenuBuilderFactory.RegisterBuilder(ABuilder: TdxPSAutoHFTextMenuBuilderClass);
begin
  if ABuilder <> nil then FBuilders.Add(ABuilder);
end;

procedure TdxPSAutoHFTextMenuBuilderFactory.UnregisterBuilder(ABuilder: TdxPSAutoHFTextMenuBuilderClass);
begin
  FBuilders.Remove(ABuilder);
end;

procedure TdxPSAutoHFTextMenuBuilderFactory.FinalizeInstance;
begin
  FreeAndNil(FBuilders);
  inherited;
end;

procedure TdxPSAutoHFTextMenuBuilderFactory.InitializeInstance;
begin
  inherited;
  FBuilders := TdxClassList.Create;
end;

function TdxPSAutoHFTextMenuBuilderFactory.GetActiveBuilder: TdxPSAutoHFTextMenuBuilderClass;
begin
  if Count <> 0 then
    Result := TdxPSAutoHFTextMenuBuilderClass(Builders[Count - 1])
  else
    Result := TdxStandardPSAutoHFTextMenuBuilder;
end;

function TdxPSAutoHFTextMenuBuilderFactory.GetBuilder(Index: Integer): TdxPSAutoHFTextMenuBuilderClass;
begin
  Result := TdxPSAutoHFTextMenuBuilderClass(FBuilders[Index]);
end;

function TdxPSAutoHFTextMenuBuilderFactory.GetCount: Integer;
begin
  Result := FBuilders.Count;
end;

{ TAbstractdxPSAutoHFTextMenuBuilder }

constructor TAbstractdxPSAutoHFTextMenuBuilder.Create;
begin
  inherited Create;
end;

class function TAbstractdxPSAutoHFTextMenuBuilder.ExtractAutoHFTextEntryIndexFromObj(Obj: TObject): Integer;
begin
  Result := 0;
end;

{ TdxStandardPSAutoHFTextMenuBuilder }

procedure TdxStandardPSAutoHFTextMenuBuilder.BuildAutoHFTextEntriesMenu(ARootItem: TObject;
  AData: Pointer; AIncludeSetupAutoHFTextEntriesItem: Boolean;
  AAutoHFTextEntries: TStrings; AOnHFTextEntriesClick, AOnSetupHFTextEntriesClick: TNotifyEvent);

  procedure AddMenuItem(AParent: TMenuItem; AIndex: Integer);
  var
    MenuItem: TMenuItem;
  begin
    MenuItem := TMenuItem.Create(AParent);
    with MenuItem do
    begin
      Caption := AAutoHFTextEntries[AIndex];
      Tag := MakeTTag(AIndex);
      OnClick := AOnHFTextEntriesClick;
    end;
    AParent.Add(MenuItem);
  end;

  procedure ClearMenuItems(AMenuItem: TMenuItem);
  var
    CurItem: TMenuItem;
  begin
    with AMenuItem do
      while Count > 0 do
      begin
        CurItem := Items[Count - 1];
        Remove(CurItem);
        CurItem.Free;
      end;
  end;

var
  MenuItem: TMenuItem;
  I: Integer;
  MI: TMenuItem;
begin
  if not (ARootItem is TMenuItem) then Exit;
  MenuItem := TMenuItem(ARootItem);
  ClearMenuItems(MenuItem);

  for I := 0 to AAutoHFTextEntries.Count - 1 do
    AddMenuItem(MenuItem, I);

  if AIncludeSetupAutoHFTextEntriesItem then
  begin
    if MenuItem.Count > 0 then MenuItem.Add(NewLine);

    MI := TMenuItem.Create(MenuItem);
    MI.Caption := cxGetResourceString(@sdxMenuInsertEditAutoTextEntries);
    MI.OnClick := AOnSetupHFTextEntriesClick;
    MenuItem.Add(MI);
  end;
end;

class function TdxStandardPSAutoHFTextMenuBuilder.ExtractAutoHFTextEntryIndexFromObj(Obj: TObject): Integer;
begin
  Result := TTagToInt(TMenuItem(Obj).Tag);
end;

initialization
  dxPSAutoHFTextMenuBuilderFactory.RegisterBuilder(TdxStandardPSAutoHFTextMenuBuilder);

end.

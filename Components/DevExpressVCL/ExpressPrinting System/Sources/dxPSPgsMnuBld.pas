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

unit dxPSPgsMnuBld;

interface

{$I cxVer.inc}

uses
  Classes, dxCore, cxClasses, dxPgsDlg, dxPSSngltn, dxBase;

type
  TdxPSPageSetupMenuBuilderClass = class of TAbstractdxPSPageSetupMenuBuilder;

  TAbstractdxPSPageSetupMenuBuilder = class
  public
    constructor Create; virtual;
    procedure BuildPageSetupMenu(ARootItem: TObject; AData: Pointer;
      AIncludeDefineItem: Boolean; AStyles: TStringList; ACurrentStyle: TBasedxPrintStyle;
      AOnStyleClick, AOnDefineStylesClick: TNotifyEvent); virtual; abstract;
    class function ExtractPrintStyleFromObj(Obj: TObject): TBasedxPrintStyle; virtual;
  end;

  TdxStandardPSPageSetupMenuBuilder = class(TAbstractdxPSPageSetupMenuBuilder)
  public
    procedure BuildPageSetupMenu(ARootItem: TObject; AData: Pointer;
      AIncludeDefineItem: Boolean; AStyles: TStringList; ACurrentStyle: TBasedxPrintStyle;
      AOnStyleClick, AOnDefineStylesClick: TNotifyEvent); override;
    class function ExtractPrintStyleFromObj(Obj: TObject): TBasedxPrintStyle; override;
  end;

  TdxPSPageSetupMenuBuilderFactory = class(TBasedxPSSingleton)
  private
    FBuilders: TdxClassList;
    function GetActiveBuilder: TdxPSPageSetupMenuBuilderClass;
    function GetBuilder(Index: Integer): TdxPSPageSetupMenuBuilderClass;
    function GetCount: Integer;
  protected
    procedure FinalizeInstance; override;
    procedure InitializeInstance; override;
  public
    class function Instance: TdxPSPageSetupMenuBuilderFactory; reintroduce; overload;
    procedure RegisterBuilder(ABuilder: TdxPSPageSetupMenuBuilderClass);
    procedure UnregisterBuilder(ABuilder: TdxPSPageSetupMenuBuilderClass);

    property ActiveBuilder: TdxPSPageSetupMenuBuilderClass read GetActiveBuilder;
    property Builders[Index: Integer]: TdxPSPageSetupMenuBuilderClass read GetBuilder;
    property Count: Integer read GetCount;
  end;

function dxPSPageSetupMenuBuilderFactory: TdxPSPageSetupMenuBuilderFactory;

implementation

uses
  Menus, SysUtils, dxPSUtl, dxPSRes;

function dxPSPageSetupMenuBuilderFactory: TdxPSPageSetupMenuBuilderFactory;
begin
  Result := TdxPSPageSetupMenuBuilderFactory.Instance;
end;

{ TdxPSAutoHFTextMenuBuilderFactory }

class function TdxPSPageSetupMenuBuilderFactory.Instance: TdxPSPageSetupMenuBuilderFactory;
begin
  Result := inherited Instance as TdxPSPageSetupMenuBuilderFactory;
end;

procedure TdxPSPageSetupMenuBuilderFactory.RegisterBuilder(ABuilder: TdxPSPageSetupMenuBuilderClass);
begin
  if ABuilder <> nil then FBuilders.Add(ABuilder);
end;

procedure TdxPSPageSetupMenuBuilderFactory.UnregisterBuilder(ABuilder: TdxPSPageSetupMenuBuilderClass);
begin
  FBuilders.Remove(ABuilder);
end;

procedure TdxPSPageSetupMenuBuilderFactory.FinalizeInstance;
begin
  FreeAndNil(FBuilders);
  inherited;
end;

procedure TdxPSPageSetupMenuBuilderFactory.InitializeInstance;
begin
  inherited;
  FBuilders := TdxClassList.Create;
end;

function TdxPSPageSetupMenuBuilderFactory.GetActiveBuilder: TdxPSPageSetupMenuBuilderClass;
begin
  if Count <> 0 then
    Result := Builders[Count - 1]
  else
    Result := TdxStandardPSPageSetupMenuBuilder;
end;

function TdxPSPageSetupMenuBuilderFactory.GetBuilder(Index: Integer): TdxPSPageSetupMenuBuilderClass;
begin
  Result := TdxPSPageSetupMenuBuilderClass(FBuilders[Index]);
end;

function TdxPSPageSetupMenuBuilderFactory.GetCount: Integer;
begin
  Result := FBuilders.Count;
end;

{ TAbstractdxPSPageSetupMenuBuilder }

constructor TAbstractdxPSPageSetupMenuBuilder.Create;
begin
  inherited Create;
end;

class function TAbstractdxPSPageSetupMenuBuilder.ExtractPrintStyleFromObj(Obj: TObject): TBasedxPrintStyle;
begin
  Result := nil;
end;

{ TStandarddxPageSetupMenuBuilder }

class function TdxStandardPSPageSetupMenuBuilder.ExtractPrintStyleFromObj(Obj: TObject): TBasedxPrintStyle;
begin
  if Obj is TMenuItem then
    Result := TBasedxPrintStyle(TTagToObj(TMenuItem(Obj).Tag))
  else
    Result := nil;
end;

procedure TdxStandardPSPageSetupMenuBuilder.BuildPageSetupMenu(ARootItem: TObject;
  AData: Pointer; AIncludeDefineItem: Boolean;
  AStyles: TStringList; ACurrentStyle: TBasedxPrintStyle;
  AOnStyleClick, AOnDefineStylesClick: TNotifyEvent);

  procedure AddMenuItem(AParent: TMenuItem; AStyle: TBasedxPrintStyle);
  var
    MenuItem: TMenuItem;
  begin
    MenuItem := TMenuItem.Create(AParent);
    with MenuItem do
    begin
      Caption := AStyle.StyleCaption;
      GroupIndex := 1;
      Hint := AStyle.Description;
      RadioItem := True;
      Tag := MakeTTag(AStyle);
      OnClick := AOnStyleClick;
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

  for I := 0 to AStyles.Count - 1 do
    AddMenuItem(MenuItem, TBasedxPrintStyle(AStyles.Objects[I]));
  if AStyles.Count > 0 then
    MenuItem[ACurrentStyle.Index].Checked := True;

  if AIncludeDefineItem then
  begin
    if MenuItem.Count > 0 then
      MenuItem.Add(NewLine);

    MI := TMenuItem.Create(MenuItem);
    MI.Caption := cxGetResourceString(@sdxDefinePrintStylesMenuItem);
    MI.Checked := False;
    MI.Tag := MakeTTag(-1);
    MI.OnClick := AOnDefineStylesClick;
    MenuItem.Add(MI);
  end;
end;

initialization
  dxPSPageSetupMenuBuilderFactory.RegisterBuilder(TdxStandardPSPageSetupMenuBuilder);

end.


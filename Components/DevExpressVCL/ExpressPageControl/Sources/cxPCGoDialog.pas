{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPageControl                                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPAGECONTROL AND ALL            }
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

unit cxPCGoDialog;

{$I cxVer.inc}

interface

uses
  Types, Windows, Menus, Classes, ImgList, cxPC, dxBuiltInPopupMenu;

type

  { TcxPCGoDialog }

  TcxPCGoDialog = class(TcxPCCustomGoDialog)
  private
    FAdapter: TdxCustomBuiltInPopupMenuAdapter;
    procedure CreateItems;
    function GetAdapter: TdxCustomBuiltInPopupMenuAdapter;
    procedure MenuItemClick(Sender: TObject);
  protected
    procedure DoClick(ATabVisibleIndex: Integer);
  public
    destructor Destroy; override;
    function Popup(X, Y: Integer): Boolean; override;
    //
    property Adapter: TdxCustomBuiltInPopupMenuAdapter read GetAdapter;
  end;

implementation

uses
  Controls, SysUtils, dxCore, cxGeometry;

type
  TcxCustomTabControlPropertiesAccess = class(TcxCustomTabControlProperties);

{ TcxPCGoDialog }

destructor TcxPCGoDialog.Destroy;
begin
  FreeAndNil(FAdapter);
  inherited Destroy;
end;

function TcxPCGoDialog.Popup(X, Y: Integer): Boolean;
begin
  CreateItems;
  Result := Adapter.Popup(cxPoint(X, Y));
end;

procedure TcxPCGoDialog.DoClick(ATabVisibleIndex: Integer);
begin
  if Assigned(OnClick) then
    OnClick(ATabVisibleIndex);
end;

procedure TcxPCGoDialog.CreateItems;

  procedure PrepareItemCaptionList(AList: TStringList);
  var
    ATabViewInfo: TcxTabViewInfo;
    I: Integer;
  begin
    for I := 0 to ViewInfo.TabsViewInfo.ViewInfoCount - 1 do
    begin
      ATabViewInfo := ViewInfo.TabsViewInfo[I];
      if ATabViewInfo.IsVisibleForGoDialog and ATabViewInfo.ActuallyEnabled then
        AList.AddObject(RemoveAccelChars(ATabViewInfo.Caption, False), ATabViewInfo);
    end;
    AList.Sorted := pcoSort in TcxCustomTabControlPropertiesAccess(TabControl.Properties).Options;
  end;

var
  AItem: TComponent;
  AItemCaptionList: TStringList;
  ATab: TcxTab;
  ATabViewInfo: TcxTabViewInfo;
  I: Integer;
begin
  AItemCaptionList := TStringList.Create;
  try
    PrepareItemCaptionList(AItemCaptionList);
    Adapter.Clear;
    Adapter.SetLookAndFeel(TabControl.GetLookAndFeel);
    Adapter.SetImages(TcxCustomTabControlPropertiesAccess(TabControl.Properties).Images);
    for I := 0 to AItemCaptionList.Count - 1 do
    begin
      ATabViewInfo := TcxTabViewInfo(AItemCaptionList.Objects[I]);
      ATab := ATabViewInfo.Tab;
      AItem := Adapter.Add(ATab.Caption, MenuItemClick, ATabViewInfo.VisibleIndex, ATab.ImageIndex);
      Adapter.SetChecked(AItem, ATab.Index = ViewInfo.TabIndex);
      Adapter.SetDefault(AItem, ATab.Index = ViewInfo.TabIndex);
    end;
  finally
    AItemCaptionList.Free;
  end;
end;

function TcxPCGoDialog.GetAdapter: TdxCustomBuiltInPopupMenuAdapter;
begin
  if FAdapter = nil then
    FAdapter := TdxBuiltInPopupMenuAdapterManager.GetActualAdapterClass.Create(nil);
  Result := FAdapter;
end;

procedure TcxPCGoDialog.MenuItemClick(Sender: TObject);
begin
  DoClick(TComponent(Sender).Tag);
end;

end.


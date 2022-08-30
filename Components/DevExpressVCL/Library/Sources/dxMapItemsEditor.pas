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

unit dxMapItemsEditor;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, cxComponentCollectionEditor,
  ImgList, cxGraphics, Menus, ActnList,
  ComCtrls, ToolWin, ExtCtrls, dxMapItem;

type
  TfrmMapItemsEditor = class(TfrmComponentCollectionEditor)
  private
    function GetMapItems: TdxMapItems;
  protected
    procedure AddItem(AItemClassIndex: Integer); override;
    procedure InitFormEditor; override;
  public
    property MapItems: TdxMapItems read GetMapItems;
  end;

implementation

{$R *.dfm}

const
  AMapItemCount = 8;
  AMapItemClasses: array [0..AMapItemCount - 1] of TdxMapItemClass = (TdxMapDot,
    TdxMapRectangle, TdxMapEllipse, TdxMapPolyline,
    TdxMapPolygon, TdxMapPath, TdxMapPushpin, TdxMapCustomElement);
  AMapItemDisplayNames: array [0..AMapItemCount - 1] of string = ('Dot',
    'Rectangle', 'Ellipse', 'Polyline', 'Polygon', 'Path', 'Pushpin', 'Custom Element');

{ TfrmComponentCollectionEditor1 }

procedure TfrmMapItemsEditor.AddItem(AItemClassIndex: Integer);
begin
  MapItems.Add(AMapItemClasses[AItemClassIndex]);
end;

procedure TfrmMapItemsEditor.InitFormEditor;
var
  ARoot, AMenuItem: TMenuItem;
  I: Integer;
begin
  inherited;
  ARoot := pmItemClasses.Items;
  for I := 0 to High(AMapItemClasses) do
  begin
    AMenuItem := TMenuItem.Create(ARoot);
    AMenuItem.Tag := I;
    AMenuItem.Caption := AMapItemDisplayNames[I];
    AMenuItem.OnClick := acAddExecute;
    ARoot.Add(AMenuItem);
  end;
end;

function TfrmMapItemsEditor.GetMapItems: TdxMapItems;
begin
  Result := Collection as TdxMapItems;
end;

end.

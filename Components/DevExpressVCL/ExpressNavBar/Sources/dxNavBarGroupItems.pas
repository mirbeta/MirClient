{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressNavBar                                            }
{                                                                    }
{           Copyright (c) 2002-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSNAVBAR AND ALL ACCOMPANYING    }
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

unit dxNavBarGroupItems;

{$I cxVer.Inc}

interface

uses
  dxNavBarCollns;

type
  TdxNavBarSeparator = class(TdxNavBarItem)
  protected
    function CanSelect: Boolean; override;
    class function GetCalculatorClass: TdxNavBarCustomGroupItemCalculatorClass; override;
  public
    procedure Draw(const APainter, ALinkViewInfo); override;
  end;

implementation

uses
  Classes, Graphics, dxNavBar;

{ TdxNavBarSeparator }

procedure TdxNavBarSeparator.Draw(const APainter, ALinkViewInfo);
begin
  TdxNavBarPainter(APainter).DrawSeparator(TdxNavBarLinkViewInfo(ALinkViewInfo));
end;

function TdxNavBarSeparator.CanSelect: Boolean;
begin
  Result := False;
end;

class function TdxNavBarSeparator.GetCalculatorClass: TdxNavBarCustomGroupItemCalculatorClass;
begin
  Result := TdxNavBarSeparatorCalculator;
end;

initialization
  RegisterClasses([TdxNavBarSeparator]);

end.


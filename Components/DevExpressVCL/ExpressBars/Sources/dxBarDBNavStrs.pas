{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars DB Navigator string table constants          }
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

unit dxBarDBNavStrs;

{$I cxVer.inc}

interface

resourcestring
  dxSBAR_DBNAVERROR1 = 'You already have an existing DBNavigator button with the same defined style';

  dxSBAR_DBNAVIGATORCATEGORYNAME = 'DB Navigator';
  dxSBAR_DELETERECORD = 'Do you want to delete the current record?';

  dxSBAR_BTNCAPTION_FIRST = 'First';
  dxSBAR_BTNCAPTION_PRIOR = 'Prior';
  dxSBAR_BTNCAPTION_NEXT = 'Next';
  dxSBAR_BTNCAPTION_LAST = 'Last';
  dxSBAR_BTNCAPTION_INSERT = 'Insert';
  dxSBAR_BTNCAPTION_DELETE = 'Delete';
  dxSBAR_BTNCAPTION_EDIT = 'Edit';
  dxSBAR_BTNCAPTION_POST = 'Post';
  dxSBAR_BTNCAPTION_CANCEL = 'Cancel';
  dxSBAR_BTNCAPTION_REFRESH = 'Refresh';

implementation

uses
  dxCore;

procedure AddBarsDBNavigatorResourceStringNames(AProduct: TdxProductResourceStrings);

  procedure InternalAdd(const AResourceStringName: string; AAddress: Pointer);
  begin
    AProduct.Add(AResourceStringName, AAddress);
  end;

begin
  InternalAdd('dxSBAR_DBNAVERROR1', @dxSBAR_DBNAVERROR1);
  InternalAdd('dxSBAR_DBNAVIGATORCATEGORYNAME', @dxSBAR_DBNAVIGATORCATEGORYNAME);
  InternalAdd('dxSBAR_DELETERECORD', @dxSBAR_DELETERECORD);
  InternalAdd('dxSBAR_BTNCAPTION_FIRST', @dxSBAR_BTNCAPTION_FIRST);
  InternalAdd('dxSBAR_BTNCAPTION_PRIOR', @dxSBAR_BTNCAPTION_PRIOR);
  InternalAdd('dxSBAR_BTNCAPTION_NEXT', @dxSBAR_BTNCAPTION_NEXT);
  InternalAdd('dxSBAR_BTNCAPTION_LAST', @dxSBAR_BTNCAPTION_LAST);
  InternalAdd('dxSBAR_BTNCAPTION_INSERT', @dxSBAR_BTNCAPTION_INSERT);
  InternalAdd('dxSBAR_BTNCAPTION_DELETE', @dxSBAR_BTNCAPTION_DELETE);
  InternalAdd('dxSBAR_BTNCAPTION_EDIT', @dxSBAR_BTNCAPTION_EDIT);
  InternalAdd('dxSBAR_BTNCAPTION_POST', @dxSBAR_BTNCAPTION_POST);
  InternalAdd('dxSBAR_BTNCAPTION_CANCEL', @dxSBAR_BTNCAPTION_CANCEL);
  InternalAdd('dxSBAR_BTNCAPTION_REFRESH', @dxSBAR_BTNCAPTION_REFRESH);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressBars DB Navigator', @AddBarsDBNavigatorResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressBars DB Navigator');

end.

{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
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

unit dxRichEdit.Utils.Cursors;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Controls;

type
  TdxRichEditCursors = class
  protected
    class constructor Initialize;
    class destructor Finalize;
  public
    class function Arrow: TCursor; static;
    class function Cross: TCursor; static;
    class function Default: TCursor; static;
    class function IBeam: TCursor; static;
    class function IBeamItalic: TCursor; static;
    class function Hand: TCursor; static;
    class function SelectRow: TCursor; static;
    class function SelectTableColumn: TCursor; static;
    class function SelectTableCell: TCursor; static;
    class function WaitCursor: TCursor; static;

    class function SizeWE: TCursor; static;
    class function SizeNS: TCursor; static;
    class function SizeNESW: TCursor; static;
    class function SizeNWSE: TCursor; static;
    class function SizeAll: TCursor; static;

    class function ResizeTableColumn: TCursor; static;
    class function ResizeTableRow: TCursor; static;

    class function BeginRotate: TCursor; static;
    class function Rotate: TCursor; static;
  end;

implementation

{$R 'dxRichEdit.Utils.Cursors.RES'}

uses
  Forms, cxLibraryConsts,
  dxRichEdit.Utils.Types;

{ TdxRichEditCursors }

class function TdxRichEditCursors.Arrow: TCursor;
begin
  Result := crArrow;
end;

class function TdxRichEditCursors.BeginRotate: TCursor;
begin
  Result := crcxRichEditControlBeginRotate;
end;

class function TdxRichEditCursors.Rotate: TCursor;
begin
  Result := crcxRichEditControlRotate;
end;

class function TdxRichEditCursors.Cross: TCursor;
begin
  Result := crCross;
end;

class function TdxRichEditCursors.Default: TCursor;
begin
  Result := crDefault;
end;

class destructor TdxRichEditCursors.Finalize;
begin
end;

class function TdxRichEditCursors.Hand: TCursor;
begin
  Result := crHandPoint;
end;

class function TdxRichEditCursors.IBeam: TCursor;
begin
  Result := crIBeam;
end;

class function TdxRichEditCursors.IBeamItalic: TCursor;
begin
  Result := crcxRichEditControlIBeamItalic;
end;

class constructor TdxRichEditCursors.Initialize;
begin
  Screen.Cursors[crcxRichEditControlBeginRotate] := LoadCursor(HInstance, 'DXRICHEDITCONTROLBEGINROTATE');
  Screen.Cursors[crcxRichEditControlIBeamItalic] := LoadCursor(HInstance, 'DXRICHEDITCONTROLIBEAMITALIC');
  Screen.Cursors[crcxRichEditControlResizeColumn] := LoadCursor(HInstance, 'DXRICHEDITCONTROLRESIZECOLUMN');
  Screen.Cursors[crcxRichEditControlResizeRow] := LoadCursor(HInstance, 'DXRICHEDITCONTROLRESIZEROW');
  Screen.Cursors[crcxRichEditControlReverseArrow] := LoadCursor(HInstance, 'DXRICHEDITCONTROLREVERSEARROW');
  Screen.Cursors[crcxRichEditControlRotate] := LoadCursor(HInstance, 'DXRICHEDITCONTROLROTATE');
  Screen.Cursors[crcxRichEditControlSelectColumn] := LoadCursor(HInstance, 'DXRICHEDITCONTROLSELECTCOLUMN');
  Screen.Cursors[crcxRichEditControlSelectTableCell] := LoadCursor(HInstance, 'DXRICHEDITCONTROLSELECTTABLECELL');
end;

class function TdxRichEditCursors.ResizeTableColumn: TCursor;
begin
  Result := crcxRichEditControlResizeColumn;
end;

class function TdxRichEditCursors.ResizeTableRow: TCursor;
begin
  Result := crcxRichEditControlResizeRow;
end;

class function TdxRichEditCursors.SelectRow: TCursor;
begin
  Result := crcxRichEditControlReverseArrow;
end;

class function TdxRichEditCursors.SelectTableColumn: TCursor;
begin
  Result := crcxRichEditControlSelectColumn;
end;

class function TdxRichEditCursors.SelectTableCell: TCursor;
begin
  Result := crcxRichEditControlSelectTableCell;
end;

class function TdxRichEditCursors.SizeAll: TCursor;
begin
  Result := crSizeAll;
end;

class function TdxRichEditCursors.SizeNESW: TCursor;
begin
  Result := crSizeNESW;
end;

class function TdxRichEditCursors.SizeNS: TCursor;
begin
  Result := crSizeNS;
end;

class function TdxRichEditCursors.SizeNWSE: TCursor;
begin
  Result := crSizeNWSE;
end;

class function TdxRichEditCursors.SizeWE: TCursor;
begin
  Result := crSizeWE;
end;

class function TdxRichEditCursors.WaitCursor: TCursor;
begin
  Result := crHourGlass;
end;

end.

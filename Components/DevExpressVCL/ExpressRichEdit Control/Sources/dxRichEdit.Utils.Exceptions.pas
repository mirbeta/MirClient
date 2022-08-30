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

unit dxRichEdit.Utils.Exceptions;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, dxCoreClasses,
  dxRichEdit.Utils.Types;

type
  TdxRichEditExceptions = class abstract
  public
    class procedure ThrowInternalException; static;
    class procedure ThrowArgumentException(const AName: string; AValue: Integer); overload; static;
    class procedure ThrowArgumentException(const AName: string; const AValue: string); overload; static;
    class procedure ThrowArgumentException(const AName: string; AValue: TObject); overload; static;
    class procedure ThrowArgumentException(const AName: string; const AValue: TSize); overload; static;
    class procedure ThrowUnsupportedFormatException; static;
    class procedure ThrowInvalidOperationException(const AName: string); overload; static;
  end;

  EdxRichEditException = class(Exception);

  EdxRichEditArgumentException = class(EdxRichEditException);

  { TdxNotImplementedException }

  TdxNotImplementedException = class(ENotImplemented)
  public
    constructor Create;
  end;

  TdxInternalException = class(Exception)
  public
    constructor Create;
  end;

  TdxRichEditInvalidFormatExceptionEventArgs = class(TdxEventArgs)
  strict private
    FException: Exception;
  public
    constructor Create(E: Exception);
    property &Exception: Exception read FException;
  end;
  TdxRichEditInvalidFormatExceptionEvent = procedure(Sender: TObject; E: TdxRichEditInvalidFormatExceptionEventArgs) of object;
  TdxRichEditInvalidFormatExceptionEventHandler = TdxMulticastMethod<TdxRichEditInvalidFormatExceptionEvent>;

  { TdxRichEditUnhandledExceptionEventArgs }

  TdxRichEditUnhandledExceptionEventArgs = class(TdxEventArgs)
  strict private
    FException: Exception;
    FHandled: Boolean;
  public
    constructor Create(E: Exception);

    property &Exception: Exception read FException;
    property Handled: Boolean read FHandled write FHandled;
  end;

  TdxRichEditUnhandledExceptionEvent = procedure(Sender: TObject; E: TdxRichEditUnhandledExceptionEventArgs) of object;
  TdxRichEditUnhandledExceptionEventHandler = TdxMulticastMethod<TdxRichEditUnhandledExceptionEvent>;

implementation

uses
  dxCore,
  dxRichEdit.Utils.Exceptions.Strs;

{ TdxRichEditExceptions }

class procedure TdxRichEditExceptions.ThrowArgumentException(const AName: string; AValue: Integer);
begin
  ThrowArgumentException(AName, IntToStr(AValue));
end;

class procedure TdxRichEditExceptions.ThrowArgumentException(const AName, AValue: string);
begin
  raise EdxRichEditException.CreateFmt(cxGetResourceString(@sdxRichEditExceptionIsNotValid), [AValue, AName]);
end;

class procedure TdxRichEditExceptions.ThrowArgumentException(const AName: string; AValue: TObject);
var
  AValueStr: string;
begin
  if AValue <> nil then
    AValueStr := AValue.ToString
  else
    AValueStr := 'nil';
  ThrowArgumentException(AName, AValueStr);
end;

class procedure TdxRichEditExceptions.ThrowArgumentException(const AName: string; const AValue: TSize);
begin
  ThrowArgumentException(AName, Format('(cx: %d, cy: %d)', [AValue.cx, AValue.cy]));
end;

class procedure TdxRichEditExceptions.ThrowInternalException;
begin
  raise EdxRichEditException.Create(cxGetResourceString(@sdxRichEditExceptionThrowInternalException));
end;

class procedure TdxRichEditExceptions.ThrowInvalidOperationException(const AName: string);
begin
  raise EInvalidOperation.Create(AName);
end;

class procedure TdxRichEditExceptions.ThrowUnsupportedFormatException;
begin
  raise EdxRichEditException.Create(cxGetResourceString(@sdxRichEditExceptionUnsupportedFormatException));
end;

{ TdxNotImplementedException }

constructor TdxNotImplementedException.Create;
begin
  inherited Create('Not implemented!');
end;

{ TdxInternalException }

constructor TdxInternalException.Create;
begin
  inherited Create('Internal error!');
end;

{ TdxRichEditInvalidFormatExceptionEventArgs }

constructor TdxRichEditInvalidFormatExceptionEventArgs.Create(E: Exception);
begin
  inherited Create;
  FException := E;
end;

{ TdxRichEditUnhandledExceptionEventArgs }

constructor TdxRichEditUnhandledExceptionEventArgs.Create(E: Exception);
begin
  inherited Create;
  FException := E;
end;


end.

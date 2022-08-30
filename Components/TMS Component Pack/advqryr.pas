{********************************************************************}
{ TADVEDIT based Query dialog component                              }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ Written by                                                         }
{   TMS Software                                                     }
{   Copyright © 2012                                                 }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{********************************************************************}

unit advqryr;

interface
{$I TMSDEFS.INC}

uses
  Classes, AdvQueryDialog;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Edits', [TAdvQueryDialog]);
end;


end.

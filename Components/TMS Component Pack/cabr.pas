{********************************************************************}
{ TCABFile component                                                 }
{ for Delphi & C++Builder                                            }
{ version 1.4                                                        }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 1999-2004                                   }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{********************************************************************}

unit cabr;

interface

uses
  CabFiles, Classes;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS System', [TCABFile]);
end;


end.


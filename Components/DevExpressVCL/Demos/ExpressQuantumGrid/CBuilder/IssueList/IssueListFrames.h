#include "Classes.hpp"
#include "IssueListForm.h"
//---------------------------------------------------------------------------

#ifndef IssueListFramesH
#define IssueListFramesH


  class TFrameInfo {
  private:
    String  FCaption;
    TfrmBasic* FFrame;
    int FID;
  public:
    TFrameInfo(int ID, String ACaption);
    void __fastcall CreateFrame(TfrmBasic* AFrame);
    void __fastcall DestroyFrame();
    void __fastcall HideFrame();
    void __fastcall ShowFrame(TWinControl* AParent);

    __property TfrmBasic* Frame = { read = FFrame };
    __property String Caption = { read = FCaption };
    __property int ID = { read = FID };
  };


  class TFrameManager {
    private:
      TFrameInfo* FActiveFrameInfo;
      TList* FFrameInfoList;
      int GetCount();
      TFrameInfo* __fastcall GetItem(int Index);
    protected:
      TFrameInfo* __fastcall GetFrameInfoByID(int AFrameID);
    public:
      TFrameManager();
      ~TFrameManager();
      void __fastcall RegisterFrame(int AFrameID, String ACaption);
      void __fastcall ShowFrame(int AFrameID, TWinControl* AParent);
      bool __fastcall CanCreate(int AFrameID);
      void __fastcall CreateFrame(int AFrameID, TfrmBasic* AFrame);

      __property TList* FrameInfoList = { read = FFrameInfoList };
      __property TFrameInfo* ActiveFrameInfo = { read = FActiveFrameInfo };
      __property int Count = { read = GetCount };
      __property TFrameInfo* Items[int Index] = { read = GetItem };
  };

  TFrameManager* FrameManager();

//---------------------------------------------------------------------------
#endif


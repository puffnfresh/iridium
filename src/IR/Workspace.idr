module IR.Workspace

import IR

manage : Eq wid => wid -> Workspace wid -> Workspace wid
manage wid (MkWorkspace Nothing) = MkWorkspace (Just (MkStack wid [] []))
manage wid (MkWorkspace (Just (MkStack wid' a b))) = MkWorkspace (Just (MkStack wid a (wid' :: b)))

# git_app

    Code
      system(paste("git ls-remote", git$url("/pak-test.git")), intern = TRUE)
    Output
      [1] "3f3b0b4ee8a0ff4563073924e5fe069da67a6d8b\tHEAD"             
      [2] "3f3b0b4ee8a0ff4563073924e5fe069da67a6d8b\trefs/heads/main"  
      [3] "cefdc0eebcd7f757efb9a80652fd8aaf1a87508e\trefs/heads/subdir"
      [4] "cefdc0eebcd7f757efb9a80652fd8aaf1a87508e\trefs/tags/v1"     


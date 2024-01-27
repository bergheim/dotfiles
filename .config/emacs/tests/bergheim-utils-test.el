(ert-deftest bergheim/test-get-and-ensure-data-dir ()
  "Test for `bergheim/get-and-ensure-data-dir`."

  ;; Backup the original value
  (let ((original-cache-dir bergheim/cache-dir))

    ;; Set the cache directory for the test
    (setq bergheim/cache-dir "/tmp/cache/")


    ;; Set the cache directory for the test
    (setq bergheim/cache-dir "/tmp/cache/")

    ;; Test if it correctly creates and returns a directory
    (should (equal (bergheim/get-and-ensure-data-dir "test-dir")
                   "/tmp/cache/test-dir"))

    ;; Test if it correctly handles nested directories
    (should (equal (bergheim/get-and-ensure-data-dir "test-dir/nested-dir")
                   "/tmp/cache/test-dir/nested-dir"))

    ;; Test if it correctly concatenates filename
    (should (equal (bergheim/get-and-ensure-data-dir "test-dir" "/file.el")
                   "/tmp/cache/test-dir/file.el"))

    ;; Test if it correctly concatenates filename without a subdir
    (should (equal (bergheim/get-and-ensure-data-dir nil "/file.el")
                   "/tmp/cache/file.el"))


    ;; Restore the original value
    (setq bergheim/cache-dir original-cache-dir)))

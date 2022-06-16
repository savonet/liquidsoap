all: build

build install clean test:
	@dune $@

shell-format:
	shfmt -f . | xargs git ls-files | xargs shfmt -i 2 -ci -sr -kp -w

shell-check:
	shfmt -f . | xargs git ls-files | xargs shfmt -i 2 -ci -sr -kp -d
	shfmt -f . | xargs git ls-files | xargs shellcheck --color=always --severity=$${SEVERITY:-style}

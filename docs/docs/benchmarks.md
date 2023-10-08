<style>
    .benchmarks-content {
        min-width: 100%;
        height: 100vh;
    }
</style>
<script src="../jquery.js"></script>
<iframe id="benchmarks-window" class="wy-nav-content-wrap benchmarks-content"></iframe>
<script>

    function sliding(l, xs, i = 0, out = []) {
        return i > xs.length - l ? out : sliding(l, xs, i + 1, [...out, xs.slice(i, i + l)])
    }

    // this is the benchmark loading part!
    (function() {
        const org = 'scalalandio'
        const repo = 'chimney-benchmark-results'
        const branch = 'main'

        const $iframe = $('#benchmarks-window')
        const $latestBuildsHolder = $('#latest-builds-holder')
        const $masterBranchHolder = $('#master-branch-holder')
        const $masterBranchMenu = $('#master-benchmarks-menu')
        const $prBenchmarksHolder = $('#pr-benchmarks-holder')
        const $prBenchmarksMenu = $('#pr-benchmarks-menu')
        const $latest = $('#latest-benchmarks')
        const $master = $('#master-benchmarks')
        const $pr = $('#pr-benchmarks')

        // I'm so sorry
        $latest.click(ev => {
            ev.preventDefault()
            $prBenchmarksHolder.removeClass('current')
            $masterBranchHolder.removeClass('current')
            $latestBuildsHolder.addClass('current')
        })

        $master.click(ev => {
            ev.preventDefault()
            $latestBuildsHolder.removeClass('current')
            $prBenchmarksHolder.removeClass('current')
            $masterBranchHolder.addClass('current')
        })

        $pr.click(ev => {
            ev.preventDefault()
            $latestBuildsHolder.removeClass('current')
            $masterBranchHolder.removeClass('current')
            $prBenchmarksHolder.addClass('current')
        })

        const addEntry = ($where, name, onClick) => {
            const el = $(`<li class="toctree-l2"><a class="reference internal">${name}</a></li>`)
            el.click(onClick)
            el.click(ev => {
                ev.preventDefault()
                $('.toctree-l2.current').removeClass('current')
                el.addClass('current')
            })
            $where.append(el)
        }

        const dataUrl = (org, repo, branch, filename) => `https://raw.githubusercontent.com/${org}/${repo}/${branch}/${filename}`
        const resultFilePath = (res) => `${res.event}/${res.sha}/${res.file}`
        const iframeUrl = (urls, topBar) => `https://jmh.morethan.io/?sources=${urls.join(',')}&topBar=${topBar}`

        const fetchResults = (org, repo, branch) => fetch(dataUrl(org, repo, branch, 'meta.json')).then(response => response.json())

        const selectResults = (results, type) =>
            results
                .filter(result => result.event === type)
                .sort((prev, next) => new Date(next.timestamp).getTime() - new Date(prev.timestamp).getTime())

        fetchResults(org, repo, branch)
            .then(resultsJson => {
                const prResults = selectResults(resultsJson, 'pull_request').slice(0, 10)
                const masterPushResults = selectResults(resultsJson, 'push').slice(0, 10)

                const masterHeadCommit = masterPushResults[0]

                const latestResults = [...masterPushResults]
                latestResults.reverse();
                const latestResultsTitle = `The latest master branch builds from ${latestResults[0].describe} to ${latestResults[latestResults.length - 1].describe}`
                const latestResultsIframeSrc = iframeUrl(latestResults.map(res => dataUrl(org, repo, branch, resultFilePath(res))), latestResultsTitle)

                $latest.click(ev => {
                  ev.preventDefault();
                  $iframe.attr('src', latestResultsIframeSrc)
                });
                $iframe.attr('src', latestResultsIframeSrc)

                sliding(2, masterPushResults) // how many master branch commits are being compared
                    .forEach(group => {
                        const thisGroup = [...group]
                        thisGroup.reverse()
                        const first = thisGroup[0]
                        const last = thisGroup[1]

                        const navBenchmarkName = `${first.describe} vs. ${last.describe}`
                        const title = `Master branch commits from ${first.describe} to ${last.describe}`
                        const iframeSrc = iframeUrl(thisGroup.map(res => dataUrl(org, repo, branch, resultFilePath(res))), title)

                        addEntry($masterBranchMenu, navBenchmarkName, ev => {
                            ev.preventDefault()
                            $iframe.attr('src', iframeSrc)
                        })
                    })

                prResults.forEach(prResult => {
                    const navBenchmarkName = `HEAD master vs. PR ${prResult.prNumber}`
                    const urls = [masterHeadCommit, prResult].map(res => dataUrl(org, repo, branch, resultFilePath(res)))
                    const title = `HEAD master commit (${masterHeadCommit.sha.substring(0, 7)}) vs. PR ${prResult.prNumber} (${prResult.prTitle})`
                    const iframeSrc = iframeUrl(urls, title)
                    addEntry($prBenchmarksMenu, navBenchmarkName, ev => {
                        ev.preventDefault()
                        $iframe.attr('src', iframeSrc)
                    })
                })
            })
    })()
</script>